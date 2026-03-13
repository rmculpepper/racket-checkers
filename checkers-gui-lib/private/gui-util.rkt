;; Copyright 2026 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base

;; ============================================================
;; Preferences

(module prefs racket/base
  (require framework/preferences)
  (provide (all-defined-out))

  (preferences:set-default 'checkers:frame:width  800 exact-positive-integer?)
  (preferences:set-default 'checkers:frame:height 600 exact-positive-integer?)
  (define pref:width  (preferences:get/set 'checkers:frame:width))
  (define pref:height (preferences:get/set 'checkers:frame:height)))

;; ============================================================
;; Cache Boxes

(module cache racket/base
  (provide cache
           cache-ref
           cache-invalidate!)

  ;; (Cache (values X...)) is (cachebox (-> (values X...)) (List X...)/#f)
  (struct cachebox (thunk [result #:mutable]) #:transparent)

  (define-syntax-rule (cache expr)
    (cachebox (lambda () expr) #f))

  (define (cache-ref cb)
    (let ([result (cachebox-result cb)])
      (if result
          (apply values result)
          (call-with-values (cachebox-thunk cb)
                            (lambda result
                              (set-cachebox-result! cb result)
                              (apply values result))))))

  (define (cache-invalidate! cb)
    (set-cachebox-result! cb #f)))

;; ============================================================
;; Output Ports

(module ports racket/base
  (require racket/match)
  (provide make-output-ports)

  ;; make-output-ports : -> (values OutputPort OutputPort (-> Content))
  ;; where Content = (Listof (cons Symbol (Listof Bytes))) -- most recent first
  (define (make-output-ports)
    (define output null)
    (define output-sema (make-semaphore 1))
    (define (make-output-collector tag)
      (define (do-write-out buf start end)
        (define subbuf (subbytes buf start end))
        (match output
          [(cons (list* (== tag) content) prev-output)
           ;; Coalesce
           (set! output (cons (list* tag subbuf content) prev-output))]
          [prev-output
           (set! output (cons (list tag subbuf) prev-output))])
        (- end start))
      (define name #f)
      (define evt output-sema)
      (define (write-out buf start end buffer? enable-break?)
        ((if enable-break? sync/enable-break sync) output-sema)
        (begin0 (do-write-out buf start end) (semaphore-post output-sema)))
      (define (close) (void))
      (define (get-write-evt buf start end)
        (wrap-evt output-sema
                  (lambda (_)
                    (begin0 (write-out buf start end #f #f)
                      (semaphore-post output-sema)))))
      (make-output-port name evt write-out close #f
                        get-write-evt #f))
    (values (make-output-collector 'output)
            (make-output-collector 'error)
            (lambda () output))))

;; ============================================================
;; Text Editor

(module editor racket/base
  (require racket/match
           racket/class
           racket/list
           racket/gui/base
           framework)
  (provide insert-text
           text:styled%
           div
           span)

  ;; insert-text : Text String StyleDelta -> Void
  (define (insert-text e text style)
    (let ([a (send e last-position)])
      (send e insert text)
      (let ([b (send e last-position)])
        (send e change-style style a b))))

  ;; A RenderV is one of
  ;; - (r:div Boolean (Listof RenderV))
  ;; - RenderH
  (struct r:div (box? content) #:prefab)

  ;; A RenderH is one of
  ;; - String
  ;; - (r:span Styles (Listof RenderH))
  (struct r:span (styles clickback content) #:prefab)

  (define (div #:box? [box? #f] . content)
    (r:div box? (filter values (flatten content))))
  (define (span #:style [style null] #:clickback [clickback #f] . content)
    (r:span (flatten style) clickback (filter values (flatten content))))

  (define text:styled%
    (class (text:wide-snip-mixin
            text:hide-caret/selection%)
      (init-field style-map)
      (inherit last-position
               change-style
               set-clickback
               insert
               delete
               get-canvas
               get-character
               set-styles-sticky
               set-autowrap-bitmap)

      (super-new (auto-wrap #t))
      (set-styles-sticky #f)
      (set-autowrap-bitmap #f)

      ;; PRE, POST: at beginning of line
      (define/public (render v)
        (let loop ([v v])
          (match v
            [(r:div #f content)
             (for ([c (in-list content)]) (loop c))]
            [(r:div #t content)
             (insert-wide-box
              (lambda (editor)
                (for ([c (in-list content)])
                  (send editor render c))))]
            [_
             (render-h v)
             (insert "\n" (last-position) 'same #f)])))

      ;; Assume v does not end with newline.
      (define/public (render-h v)
        (match v
          [(r:span styles clickback content)
           (define a (last-position))
           (for ([c (in-list content)])
             (render-h c))
           (define b (last-position))
           (when clickback (set-clickback a b clickback))
           (change-styles a b styles)]
          [(? (lambda (v) (is-a? v snip%)) sn)
           (insert sn (last-position) 'same #f)]
          [(? string? s)
           (insert s (last-position) 'same #f)])
        (void))

      ;; insert/styles : (Listof (U Symbol StyleDelta)) String ... -> Void
      ;; A list of styles to be applied. The first style is the last applied.
      (define/public (insert/styles styles . texts)
        (unless (andmap (lambda (x) (or (string? x) (is-a? x snip%))) texts)
          (raise-type-error 'insert/styles "list of strings" texts))
        (let-values ([(a b) (put texts)])
          (change-styles a b (reverse styles))))

      ;; insert/styles+click : (Listof (U Symbol StyleDelta)) (?? -> Void) String ...
      ;;                    -> Void
      (define/public (insert/styles+click styles clickback . texts)
        (unless (andmap (lambda (x) (or (string? x) (is-a? x snip%))) texts)
          (raise-type-error 'insert/styles+click "list of strings" texts))
        (let-values ([(a b) (put texts)])
          (change-styles a b (reverse styles))
          (set-clickback a b clickback)))

      ;; put : (Listof String) -> (values Nat Nat)
      (define/private (put texts)
        (let ([a (last-position)])
          (let loop ([texts texts] [where a])
            (if (pair? texts)
                (begin (insert (car texts) where 'same #f)
                       (loop (cdr texts) (last-position)))
                (values a where)))))

      ;; change-styles : Nat Nat (Listof (U Symbol StyleDelta)) -> Void
      (define/private (change-styles a b styles)
        (for ([style (in-list styles)])
          (let ([style (if (symbol? style) (hash-ref style-map style #f) style)])
            (when style (change-style style a b)))))

      ;; newline : -> Void
      (define/public (newline)
        (insert/styles '() "\n"))

      ;; insert-wide-box : (TextStyled -> Void) -> Void
      (define/public (insert-wide-box p)
        (internal-insert-box p #t)
        (newline))

      ;; internal-insert-box : (TextStyled -> Void) Boolean -> Void
      (define/private (internal-insert-box proc wide?)
        (define seditor (new text:styled% (style-map style-map)))
        (define snip (new editor-snip% (editor seditor)))
        (send seditor begin-edit-sequence #f)
        (proc seditor)
        (send seditor delete-final-newline)
        (send seditor end-edit-sequence)
        (send seditor lock #t)
        (let [(canvas (get-canvas))]
          (when (and wide? (is-a? canvas canvas:wide-snip<%>))
            (send canvas add-wide-snip snip)))
        (insert snip))

      (define/public (delete-final-newline)
        (define p (last-position))
        (when (and (> p 0) (eqv? (get-character (sub1 p)) #\newline))
          (delete (sub1 p) p)))

      )))

;; ============================================================
;; Styles

(module styles racket/base
  (require racket/class
           racket/gui/base)
  (provide (all-defined-out))

  (define style:no-change (make-object style-delta% 'change-nothing))
  (define style:normal (make-object style-delta% 'change-normal))
  (define style:bold (make-object style-delta% 'change-weight 'bold))
  (define style:italic (make-object style-delta% 'change-italic))

  (define style:large (make-object style-delta%))
  (void (send style:large set-size-mult 1.5))

  (define style:blue (make-object style-delta%))
  (void (send style:blue set-delta-foreground "Blue"))

  (define style:red (make-object style-delta%))
  (void (send style:red set-delta-foreground "Red"))

  (define style:orange (make-object style-delta%))
  (void (send style:orange set-delta-foreground "Orange"))

  (define style:green (make-object style-delta%))
  (void (send style:green set-delta-foreground "ForestGreen"))

  (define style:purple (make-object style-delta%))
  (void (send style:purple set-delta-foreground "Purple"))

  (define style:gray (make-object style-delta%))
  (void (send style:gray set-delta-foreground "DimGray"))

  (define style:darkblue (make-object style-delta%))
  (void (send style:darkblue set-delta-foreground "DarkBlue"))

  (define style:clickback (make-object style-delta% 'change-underline #t))
  (void (send style:clickback set-delta-foreground "blue"))

  ;; StyleMap = (Hasheq Symbol StyleDelta)

  (define basic-styles
    (hasheq
     'no-change style:no-change
     'normal    style:normal
     'bold      style:bold
     'italic    style:italic
     'large     style:large
     'clickback style:clickback
     'red       style:red
     'orange    style:orange
     'blue      style:blue
     'green     style:green
     'purple    style:purple
     'darkblue  style:darkblue
     'gray      style:gray
     'error     style:red
     'value     style:darkblue)))

;; ============================================================

