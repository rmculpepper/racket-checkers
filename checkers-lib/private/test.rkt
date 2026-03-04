#lang racket/base
(require racket/match
         racket/string
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
(define (test* name loc opts proc)
  (define frame (test-frame name loc opts))
  (define ctx (cons frame (current-test-context)))
  (parameterize ((current-test-context ctx))
    (with-handlers ([stop-test?
                     (lambda (st)
                       (test-notify ctx st))])
      (test-notify ctx 'start)
      (call-with-continuation-barrier proc)
      (test-notify ctx 'end)))
  (void))

;; test-notify : TestContext Event -> Void
(define (test-notify ctx event)
  (define listeners (current-test-listeners))
  (define (notify-all event)
    (for ([listener (in-list listeners)])
      (listener ctx event)))
  (notify-all event)
  (match event
    ['end
     (notify-all (if (test-context-xfail? ctx) 'uxpass 'xfail))]
    [(? check-failure?)
     (notify-all (if (test-context-xfail? ctx) 'xfail 'fail))]
    [(? skip-test?)
     (notify-all 'skip)]
    [_ (void)]))

;; TestContext = (Listof TestFrame)

(define current-test-context (make-parameter null))

(define (test-context-full-name-line ctx)
  (let ([line (test-context-full-name ctx)])
    (if line (string-append line "\n") "")))

(define (test-context-full-name ctx)
  (define (frame-name fr)
    (or (test-frame-name fr)
        (let ([loc (test-frame-loc fr)])
          (and loc (source-location->string loc)))))
  (match (filter string? (map test-frame-name ctx))
    ['() #f]
    [names (string-join names " > ")]))

(define (test-context-xfail? ctx)
  (match ctx
    [(cons (test-frame _ _ opts) _)
     (and (memq 'xfail opts) #t)]
    [_ #f]))

;; TestFrame = (test-frame String/#f Srcloc/#f (Listof TestOption))
;; where TestOption is one of the following:
;; - 'xfail     -- indicates test is expected to fail
(struct test-frame (name loc opts) #:transparent)

;; ----------------------------------------
;; Test Listeners

;; TestListener = (TestContext Event -> Void)

;; Event is one of
;; - 'start
;; - 'end
;; - CheckFailure
;; - SkipTest
;; - SynthEvent

;; SynthEvent is one of
;; - 'pass      -- passed as expected
;; - 'fail      -- failed (unexpected)
;; - 'xfail     -- failed as expected
;; - 'uxpass    -- passed when expected to fail
;; - 'skip      -- skipped

;; make-test-listener : TestListener
(define (make-test-listener #:print-levels [print-levels 0]
                            #:print-xfail? [print-xfail? #t]
                            #:print-skip? [print-skip? #f])
  (define level 0)
  (define (listener ctx event)
    (match event
      ['start
       (when (< level print-levels)
         (printf "~a~a\n"
                 (make-string (* 2 level) #\space)
                 (or (test-context-full-name ctx) "(unnamed test)")))
       (set! level (add1 level))]
      ['end
       (when (test-context-xfail? ctx)
         (print-uxpass ctx))
       (set! level (sub1 level))]
      [(? check-failure? cf)
       (cond [(test-context-xfail? ctx)
              (when print-xfail?
                (print-fail ctx cf "EXPECTED FAILURE"))]
             [else (print-fail ctx cf "FAILURE")])
       (set! level (sub1 level))]
      [(? skip-test? sk)
       (when print-skip?
         (print-skip ctx sk))
       (set! level (sub1 level))]
      [_ (void)]))
  listener)

(define (print-fail ctx cf what)
  (match-define (check-failure info-stack info) cf)
  (define (print-info key label mode #:if [ok? void] #:map [f values])
    (match (assoc key info)
      [(list _ v) (printkv label mode (f v))]
      [#f (void)]))
  (printf "~a~a\n~a\n" bar-separator (test-context-full-name-line ctx) what)
  (print-info '#:location "location" 'display
              #:if source-location? #:map source-location->string)
  (print-info '#:actual "actual" 'value #:map result->print-result)
  (print-info '#:expectvs "expected" 'value #:map result->print-result)
  (print-info '#:expected "expected" 'display #:if string?)
  (for ([e (in-list info)] #:when (or (symbol? (car e)) (string? (car e))))
    (printkv (format "~a" (car e)) 'value (cadr e)))
  (print-info '#:failure "failure" 'display #:if string?)
  (print-info '#:subfail "detail" 'display #:if string?)
  (write-string bar-separator)
  (void))

(define (print-uxpass ctx)
  (define name-line (test-context-full-name-line ctx))
  (printf "~a~aUNEXPECTED PASS\n~a" bar-separator name-line bar-separator))

(define (print-skip ctx sk)
  (define name-line (test-context-full-name-line ctx))
  (printf "~a~aSKIPPED\n~a" bar-separator name-line bar-separator))

(define (printkv k mode v)
  (define KEYLEN 12)
  (define key (format "~a: " k))
  (define pad (make-string (max 0 (- KEYLEN (string-length key))) #\space))
  (case mode
    ((display)
     (printf "~a~a~a\n" key pad v))
    ((value)
     (printf "~a~a~e\n" key pad v))))

(define bar-separator "--------------------")

;; raco-test-listener : TestListener
(define (raco-test-listener ctx event)
  (case event
    [(pass xfail) (test-log! #t)]
    [(fail uxpass) (test-log! #f)]
    [else (void)]))

;; current-test-listeners : Parameter of (Listof TestListener)
(define current-test-listeners
  (make-parameter
   (list (make-test-listener)
         raco-test-listener)))
