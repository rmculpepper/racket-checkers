;; Copyright 2026 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/match
         racket/string
         racket/path
         syntax/srcloc
         raco/testing
         "check.rkt"
         "result.rkt")
(provide (all-defined-out))

;; ----------------------------------------
;; Test

;; Tests only catch raises from check and skip-test; other exceptions indicate
;; bugs in test program. Test does not detect continuation escapes.

;; test* : String/#f Srcloc/#f (Listof TestOption) (-> Any) -> Void
(define (test* name loc proc)
  (define frame (test-frame name loc))
  (define ctx (cons frame (current-test-context)))
  (parameterize ((current-test-context ctx))
    (with-handlers ([test-signal?
                     (lambda (st)
                       (test-notify ctx st))])
      (test-notify ctx 'start)
      (call-with-continuation-barrier proc)
      (test-notify ctx 'end)))
  (void))

;; test-notify : TestContext Event -> Void
(define (test-notify ctx event)
  (for ([listener (in-list (current-test-listeners))])
    (listener ctx event)))

;; ----------------------------------------
;; Test Contexts

;; TestContext = (Listof TestFrame)
;; TestFrame = (test-frame String/#f Srcloc/#f)
(struct test-frame (name loc) #:transparent)

;; current-test-context : Parameter of TestContext
(define current-test-context (make-parameter null))

(define (test-context-full-name-line ctx)
  (let ([line (test-context-full-name ctx)])
    (if line (string-append line "\n") "")))

(define (test-context-full-name ctx)
  (match (filter string? (map test-frame-short-name (reverse ctx)))
    ['() #f]
    [names (string-join names " > ")]))

(define (test-context-short-name ctx)
  (and (pair? ctx) (test-frame-short-name (car ctx))))

(define (test-frame-short-name fr)
  (or (test-frame-name fr)
      (loc->short-name (test-frame-loc fr))))

(define (loc->short-name loc)
  (and (source-location-known? loc)
       (let ([src (source-location-source loc)])
         (define src* (if (path? src) (file-name-from-path src) src))
         (source-location->string (update-source-location loc #:source src*)))))

;; ----------------------------------------
;; Test Listeners

;; TestListener = (TestContext Event -> Void)

;; Event is one of
;; - 'start
;; - 'end
;; - CheckFailure
;; - SkipTest

;; TestState is one of 'pass, 'fail, 'skip, 'incomplete
;; and 'incomplete means no test end reported.

;; make-test-listener : ... -> TestListener
(define (make-test-listener #:out [out (current-error-port)] ;; OutPort or (-> OutPort)
                            #:tell-raco? [tell-raco? #t]
                            #:trace [print-levels 0]
                            #:print-states [print-states '(fail)])
  (define (get-out) (if (procedure? out) (out) out))
  (define cv (make-counter-vector))
  (define (tell s)
    (counter-incr! cv s 1)
    (when tell-raco? (tell-raco s)))
  (define (listener ctx event)
    (match event
      ;; --------------------
      ;; Test events
      ['start
       (tell 'start)
       (define level (max 0 (sub1 (length ctx)))) ;; though ctx should be non-empty
       (when (< level print-levels)
         (printf "~a~a\n"
                 (make-string (* level 2) #\space)
                 (or (test-context-short-name ctx) "?")))]
      ['end
       (tell 'pass)
       (when (memq 'pass print-states)
         (print-pass ctx (get-out)))]
      [(check-failure info)
       (tell 'fail)
       (when (memq 'fail print-states)
         (print-fail ctx info (get-out)))]
      [(skip-test-signal info)
       (tell 'skip)
       (when (memq 'skip print-states)
         (print-skip ctx info (get-out)))]
      ;; --------------------
      ;; Query methods (ctx unused)
      ['get-counters
       (define h
         (for/hasheq ([key (in-vector counter-slots)] [n (in-vector cv)])
           (values key n)))
       (define incomplete
         (- (hash-ref h 'start)
            (for/sum ([(k n) (in-hash h)] #:when (memq k end-states)) n)))
       (hash-set h 'incomplete incomplete)]
      [_ (void)]))
  listener)

(define end-states
  '(pass fail skip))
(define counter-slots
  '#(start pass fail skip))
(define (make-counter-vector)
  (make-vector (vector-length counter-slots) 0))
(define (slot-index s)
  (for/or ([i (in-naturals)] [cs (in-vector counter-slots)] #:when (eq? cs s)) i))
(define (counter-incr! cv s delta)
  (define si (slot-index s))
  (let loop ()
    (define n (vector-ref cv si))
    (if (vector-cas! cv si n (+ n delta)) (void) (loop))))

(define (tell-raco s)
  (case s
    [(pass) (test-log! #t)]
    [(fail) (test-log! #f)]
    [else (void)]))

(define (print-pass ctx out)
  (write-string bar-line out)
  (write-string (test-context-full-name-line ctx) out)
  (write-string "PASS\n" out)
  (write-string bar-line out)
  (void))

(define (print-skip ctx info out)
  (write-string bar-line out)
  (write-string (test-context-full-name-line ctx) out)
  (write-string "SKIP\n" out)
  (write-string bar-line out)
  (void))

(define (print-fail ctx info out)
  (define (print-info key label mode #:if [ok? void] #:map [f values])
    (match (assoc key info)
      [(list _ v) (when (ok? v) (printkv label mode (f v) out))]
      [#f (void)]))
  (write-string bar-line out)
  (write-string (test-context-full-name-line ctx) out)
  (write-string "FAIL\n" out)
  (print-info '#:location "location" 'display
              #:if source-location? #:map source-location->string)
  (print-info '#:actual "actual" 'value #:map result->print-result)
  (print-info '#:expected "expected" 'display #:if string?)
  (print-info '#:expectvs "expected" 'value #:map result->print-result)
  (print-info '#:othervs "other" 'value #:map result->print-result)
  (for ([e (in-list info)] #:when (or (symbol? (car e)) (string? (car e))))
    (printkv (format "~a" (car e)) 'value (cadr e) out))
  (print-info '#:failure "failure" 'display #:if string?)
  (print-info '#:subfail "detail" 'display #:if string?)
  (write-string bar-line out)
  (void))

(define (printkv k mode v out)
  (define KEYLEN 12)
  (define key (format "~a: " k))
  (define pad (make-string (max 0 (- KEYLEN (string-length key))) #\space))
  (case mode
    ((display)
     (fprintf out "~a~a~a\n" key pad v))
    ((value)
     (fprintf out "~a~a~e\n" key pad v))))

(define bar-line "--------------------\n")

;; current-test-listeners : Parameter of (Listof TestListener)
(define current-test-listeners
  (make-parameter
   (list (make-test-listener #:out current-error-port))))
