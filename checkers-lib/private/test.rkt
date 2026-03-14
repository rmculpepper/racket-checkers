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

;; Tests only catch raises from check; other exceptions indicate
;; bugs in test program. Test does not detect continuation escapes.

;; test* : String/#f Srcloc/#f (Listof TestOption) (-> Any) -> Void
(define (test* name loc proc)
  (define frame (test-frame name loc #f))
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
;; TestFrame = (test-frame String/#f Srcloc/#f Bool)
(struct test-frame (name loc [fail? #:mutable]) #:transparent)

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

(define (test-context-fail! ctx)
  (when (pair? ctx)
    (unless (test-frame-fail? (car ctx))
      (set-test-frame-fail?! (car ctx) #t)
      (test-context-fail! (cdr ctx)))))

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

;; TestState is one of 'pass, 'fail, 'incomplete
;; and 'incomplete means no test end reported.

;; make-test-listener : ... -> TestListener
(define (make-test-listener #:out [out (current-error-port)] ;; OutPort or (-> OutPort)
                            #:tell-raco? [tell-raco? #t]
                            #:progress [progress #f]
                            #:print-states [print-states '(fail)])
  (define (get-out) (if (procedure? out) (out) out))
  (define cv (make-counter-vector))
  (define (tell s)
    (counter-incr! cv s 1)
    (when tell-raco? (tell-raco s)))
  (define (tell-progress ctx)
    (progress ctx (vector-ref cv pass-index) (vector-ref cv fail-index)))
  (define (listener ctx event)
    (match event
      ;; --------------------
      ;; Test events
      ['start
       (tell 'start)
       (when progress
         (tell-progress ctx))]
      ['end
       (tell 'pass)
       (when (memq 'pass print-states)
         (when progress (progress))
         (print-pass ctx (get-out)))
       (when progress (tell-progress (cdr ctx)))]
      [(check-failure info)
       (test-context-fail! ctx)
       (tell 'fail)
       (when (memq 'fail print-states)
         (when progress (progress))
         (print-fail ctx info (get-out)))
       (when progress (tell-progress (cdr ctx)))]
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
  '(pass fail))
(define counter-slots
  '#(start pass fail))
(define (make-counter-vector)
  (make-vector (vector-length counter-slots) 0))
(define (slot-index s)
  (for/or ([i (in-naturals)] [cs (in-vector counter-slots)] #:when (eq? cs s)) i))
(define (counter-incr! cv s delta)
  (define si (slot-index s))
  (let loop ()
    (define n (vector-ref cv si))
    (if (vector-cas! cv si n (+ n delta)) (void) (loop))))
(define pass-index (slot-index 'pass))
(define fail-index (slot-index 'fail))

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

(define (print-fail ctx info out)
  (write-string bar-line out)
  (write-string (test-context-full-name-line ctx) out)
  (write-string "FAIL\n" out)
  (print-fault info (lambda (k mode v) (printkv k mode v out)))
  (write-string bar-line out)
  (void))

(define (printkv k mode v out)
  (define KEYLEN 12)
  (define key (format "~a: " k))
  (define pad (make-string (max 0 (- KEYLEN (string-length key))) #\space))
  (case mode
    ((display)
     (fprintf out "~a~a~a\n" key pad v))
    ((write)
     (fprintf out "~a~a~s\n" key pad v))
    ((value)
     (fprintf out "~a~a~e\n" key pad v))))

(define bar-line "--------------------\n")

;; current-test-listeners : Parameter of (Listof TestListener)
(define current-test-listeners
  (make-parameter
   (list (make-test-listener #:out current-error-port))))
