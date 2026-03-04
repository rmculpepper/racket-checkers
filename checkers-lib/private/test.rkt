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
  (for ([listener (in-list (current-test-listeners))])
    (listener ctx event)))

;; TestContext = (Listof TestFrame)

(define current-test-context (make-parameter null))

(define (test-context-full-name-line ctx)
  (let ([line (test-context-full-name ctx)])
    (if line (string-append line "\n") "")))

(define (test-context-full-name ctx)
  (match (filter string? (map test-frame-short-name ctx))
    ['() #f]
    [names (string-join names " > ")]))

(define (test-context-short-name ctx)
  (and (pair? ctx) (test-frame-short-name (car ctx))))

(define (test-frame-short-name fr)
  (or (test-frame-name fr)
      (let ([loc (test-frame-loc fr)])
        (and loc (source-location->string loc)))))

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

;; make-test-listener : ... -> TestListener
(define (make-test-listener #:tell-raco? [tell-raco? #t]
                            #:print-names [print-levels 0]
                            #:print-states [print-states '(fail xfail uxpass)])
  (define (tell-raco s)
    (when tell-raco? (tell-raco* s)))
  (define (listener ctx event)
    (match event
      ['start
       (define level (max 0 (sub1 (length ctx)))) ;; though ctx should be non-empty
       (when (< level print-levels)
         (printf "~a~a\n"
                 (make-string (* level 2) #\space)
                 (or (test-context-short-name ctx) "?")))]
      ['end
       (define state (if (test-context-xfail? ctx) 'uxpass 'pass))
       (tell-raco state)
       (when (memq state print-states)
         (print-pass ctx state))]
      [(? check-failure? cf)
       (define state (if (test-context-xfail? ctx) 'xfail 'fail))
       (cond [(test-context-xfail? ctx)
              (tell-raco 'xfail)
              (when (memq 'xfail print-states)
                (print-fail ctx cf "FAILURE (EXPECTED)"))]
             [else
              (tell-raco 'fail)
              (when (memq 'fail print-states)
                (print-fail ctx cf "FAILURE"))])]
      [(? skip-test? sk)
       (tell-raco 'skip)
       (when (memq 'skip print-states)
         (print-skip ctx sk))]
      [_ (void)]))
  listener)

(define (tell-raco* s)
  (case s
    [(pass xfail) (test-log! #t)]
    [(fail uxpass) (test-log! #f)]
    [(skip) (void)]))

(define (print-pass ctx uxpass?)
  (write-string bar-line)
  (write-string (test-context-full-name-line ctx))
  (cond [uxpass? (write-string "PASS (UNEXPECTED)\n")]
        [else (write-string "PASS\n")])
  (write-string bar-line)
  (void))

(define (print-skip ctx sk)
  (write-string bar-line)
  (write-string (test-context-full-name-line ctx))
  (write-string "SKIPPED\n")
  (write-string bar-line)
  (void))

(define (print-fail ctx cf xfail?)
  (match-define (check-failure info) cf)
  (define (print-info key label mode #:if [ok? void] #:map [f values])
    (match (assoc key info)
      [(list _ v) (printkv label mode (f v))]
      [#f (void)]))
  (write-string bar-line)
  (write-string (test-context-full-name-line ctx))
  (cond [xfail? (write-string "FAILURE (EXPECTED)\n")]
        [else (write-string "FAILURE\n")])
  (print-info '#:location "location" 'display
              #:if source-location? #:map source-location->string)
  (print-info '#:actual "actual" 'value #:map result->print-result)
  (print-info '#:expectvs "expected" 'value #:map result->print-result)
  (print-info '#:expected "expected" 'display #:if string?)
  (for ([e (in-list info)] #:when (or (symbol? (car e)) (string? (car e))))
    (printkv (format "~a" (car e)) 'value (cadr e)))
  (print-info '#:failure "failure" 'display #:if string?)
  (print-info '#:subfail "detail" 'display #:if string?)
  (write-string bar-line)
  (void))

(define (printkv k mode v)
  (define KEYLEN 12)
  (define key (format "~a: " k))
  (define pad (make-string (max 0 (- KEYLEN (string-length key))) #\space))
  (case mode
    ((display)
     (printf "~a~a~a\n" key pad v))
    ((value)
     (printf "~a~a~e\n" key pad v))))

(define bar-line "--------------------\n")

;; current-test-listeners : Parameter of (Listof TestListener)
(define current-test-listeners
  (make-parameter
   (list (make-test-listener))))
