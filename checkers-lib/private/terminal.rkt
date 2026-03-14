;; Copyright 2026 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/match
         racket/list
         (only-in '#%terminal
                  terminal-init
                  terminal-get-screen-size
                  terminal-carriage-return
                  terminal-clear
                  terminal-move-cursor
                  terminal-write-char
                  terminal-flush
                  terminal-set-color))
(provide (all-defined-out))

(define DEFAULT-BG 'dark-gray)

(define (make-terminal-status #:bgcolor [bgcolor #f])
  (define in (current-input-port))
  (define out (current-output-port))
  (define err (current-error-port))
  (cond [(and (terminal-port? in)
              (terminal-port? out)
              (terminal-init -1 -1))
         (let ([bg (symbol->color-code (or bgcolor DEFAULT-BG))])
           (make-terminal-status* in out err bg))]
        [else #f]))

(define (make-terminal-status* in out err bgcolor)
  (define (get-positions) (cons (file-position out) (file-position err)))
  (define (at-line-start?) (and (port-at-line-start? out) (port-at-line-start? err)))
  (define last-positions #f)
  (define last-screen-size #f)
  (define last-line empty-linebuf)

  (define (reset-last-state!)
    (set! last-screen-size #f)
    (set! last-positions #f)
    (set! last-line empty-linebuf))

  ;; retract : -> Boolean
  (define (retract)
    (cond [(not last-positions)
           ;; nothing to retract
           #t]
          [(equal? (get-positions) last-positions)
           (define columns (cdr (terminal-get-screen-size)))
           (define lines-up
             (cond [(equal? (terminal-get-screen-size) last-screen-size) 1]
                   [else (max 1 (quotient/ceiling (linebuf-length last-line) columns))]))
           (terminal-carriage-return) ;; should be redundant
           (terminal-move-cursor 'up lines-up)
           (terminal-clear 'eos)
           (reset-last-state!)
           #t]
          [else #f]))

  ;; put : LineBuf LineBuf -> (U 'complete 'incomplete #f)
  (define (put left [right empty-linebuf])
    (define current-positions (get-positions))
    (cond [(equal? current-positions last-positions)
           (cond [(equal? (terminal-get-screen-size) last-screen-size)
                  (put/update left right)]
                 [else
                  (retract)
                  (put/fresh left right)])]
          [else (put/fresh left right)]))

  (define (put/fresh left right)
    (unless (at-line-start?) (fprintf out "\n"))
    (fprintf out "\n")
    (set! last-screen-size (terminal-get-screen-size))
    (set! last-positions (get-positions))
    (set! last-line empty-linebuf)
    (put/update left right))

  (define (put/update left right)
    ;; PRE: cursor is on blank line below update area
    (define-values (line status)
      (match last-screen-size
        [(cons _ columns)
         (compose-line left right columns)]))
    (when status
      (terminal-carriage-return) ;; should be redundant
      (terminal-move-cursor 'up 1)
      (set-colors default-color bgcolor)
      (define-values (start end clear?)
        (line-get-update-range last-line line))
      (match line
        [(linebuf s efgs)
         (terminal-move-cursor 'right start)
         (for/fold ([last-fg default-color])
                   ([c (in-string s start end)]
                    [efg (in-bytes efgs start end)])
           (define fg (decode-color efg))
           (unless (= fg last-fg) (set-colors fg bgcolor))
           (terminal-write-char c)
           fg)
         (terminal-move-cursor 'right (- (string-length s) end))])
      (when clear? (terminal-clear 'eol))
      (set-colors default-color default-color)
      (terminal-carriage-return)
      (terminal-move-cursor 'down 1)
      (set! last-line line))
    (set! last-positions (get-positions))
    status)

  (define (terminal-status . cmd)
    (begin0
        (match cmd
          [(list 'retract) (retract)]
          [(list 'update left) (put left empty-linebuf)]
          [(list 'update left right) (put left right)]
          [_ (error 'terminal-status "bad command: ~e" cmd)])
      (terminal-flush)))
  terminal-status)

(define (quotient/ceiling n d)
  (quotient (+ n d -1) d))

(define (port-at-line-start? p)
  (define-values (line col pos) (port-next-location p))
  (and col (zero? col)))

(define (set-colors fg bg)
  ;; It seems setting fg resets bg (?), so always set both.
  (terminal-set-color fg #f)
  (terminal-set-color bg #t))

(define (symbol->color-code sym)
  (case sym
    ;; from expeditor/private/color:
    [(default) -1]
    [(black) 0]
    [(red) 1]
    [(green) 2]
    [(yellow) 3]
    [(blue) 4]
    [(magenta) 5]
    [(cyan) 6]
    [(light-gray) 7]
    [(dark-gray) 8]
    [(light-red) 9]
    [(light-green) 10]
    [(light-yellow) 11]
    [(light-blue) 12]
    [(light-magenta) 13]
    [(light-cyan) 14]
    [(white) 15]
    [else (error 'symbol->color-code "bad color: ~e" sym)]))

(define default-color -1)

(define (encode-color fg) (+ fg 1))
(define (decode-color efg) (- efg 1))
(define enc-default-color 0)

;; ----------------------------------------

(define ABBREV-S "… ")
(define ABBREV-LEN (string-length ABBREV-S))
(define ABBREV-EFG (make-bytes ABBREV-LEN enc-default-color))

;; LineBuf = (linebuf String Bytes), same length
(struct linebuf (s efg) #:prefab)
(define empty-linebuf (linebuf "" #""))

(define (linebuf-length lb)
  (string-length (linebuf-s lb)))

(define (string->linebuf s [fg 'default])
  (define len (string-length s))
  (linebuf s (make-bytes len (encode-color (symbol->color-code fg)))))

(define (linebuf-append . lbs)
  (linebuf (apply string-append (map linebuf-s lbs))
           (apply bytes-append (map linebuf-efg lbs))))

(define (linebuf-join #:sep [sep-lb empty-linebuf]
                      #:prefix [prefix-lb empty-linebuf]
                      lbs)
  (apply linebuf-append (cons prefix-lb (add-between lbs sep-lb))))

;; compose-line : LineBuf LineBuf Nat -> (values LineBuf (U 'complete 'incomplete #f))
(define (compose-line left right columns)
  (match-define (linebuf left-s left-efg) left)
  (match-define (linebuf right-s right-efg) right)
  (define left-len (string-length left-s))
  (define right-len (string-length right-s))
  (cond [(< (+ left-len right-len 1) columns)
         (define padlen (- columns left-len right-len))
         (define s (string-append left-s (make-string padlen #\space) right-s))
         (define efg (bytes-append left-efg (make-bytes padlen enc-default-color) right-efg))
         (values (linebuf s efg) 'complete)]
        [(< (+ ABBREV-LEN right-len) columns)
         (define left-len* (- columns right-len ABBREV-LEN))
         (define s (string-append (substring left-s 0 left-len*) ABBREV-S right-s))
         (define efg (bytes-append (subbytes left-efg 0 left-len*) ABBREV-EFG right-efg))
         (values (linebuf s efg) 'incomplete)]
        [else (values empty-linebuf #f)]))

;; line-get-update-range : LineBuf LineBuf -> (values Nat Nat Boolean)
(define (line-get-update-range oldlb newlb)
  (match-define (linebuf old-s old-efg) oldlb)
  (match-define (linebuf new-s new-efg) newlb)
  (define (same-at i)
    (and (eqv? (string-ref old-s i) (string-ref new-s i))
         (= (bytes-ref old-efg i) (bytes-ref new-efg i))))
  (define oldlen (string-length old-s))
  (define newlen (string-length new-s))
  (define start
    (let loop ([i 0])
      (cond [(and (< i oldlen) (< i newlen) (same-at i))
             (loop (add1 i))]
            [else i])))
  (define end
    (cond [(= oldlen newlen)
           (let loop ([i (sub1 newlen)])
             (cond [(and (>= i start) (same-at i))
                    (loop (sub1 i))]
                   [else (add1 i)]))]
          [else (max oldlen newlen)]))
  (values start (min end newlen) (> end newlen)))
