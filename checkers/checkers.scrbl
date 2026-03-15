#lang scribble/manual
@(require scribble/example
          (for-label racket/base
                     racket/contract
                     raco/testing
                     syntax/srcloc
                     checkers))

@title{checkers: Testing Framework}
@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@(define the-eval (make-base-eval))
@(the-eval '(require checkers racket/list))

@defmodule[checkers]

This library provides a simple testing framework.

@; ------------------------------------------------------------
@section[#:tag "intro"]{Introduction to Checkers}

A test is written as a @racket[test] expression, usually containing one or more
@racket[check] expressions. Tests are actions, not values: a @racket[test]
expression's body is immediately executed, and the test expression returns
@racket[(void)]. Tests may be anonymous or named.

@examples[#:eval the-eval #:label #f
(test (check (+ 5 -5) #:is 0))
(test #:name "identity"
  (check (+ 7 0) #:is 7)
  (check (* 8 1) #:is 8))
]

If a check fails, information about the failure and the enclosing test is
printed and the execution of the enclosing test stops.

@examples[#:eval the-eval #:label #f
(test #:name "addition"
  (check (+ 1 1) #:is 2)
  (test #:name "more addition"
    (check (+ 2 2) #:is 5) (code:comment "whoops")
    (printf "this is not printed\n"))
  (printf "but this line is\n"))
]

The @racket[check] form catches exceptions and multiple values in the ``actual''
expression and supports several kinds of assertions about its result.

@examples[#:eval the-eval #:label #f
(test #:name "arithmetic"
  (check (+ 1 2) #:is 3)
  (check (+ 4 6) #:with even?)
  (check (quotient/remainder 10 3) #:is (values 3 1))
  (check (/ 1 0) #:error exn:fail:contract:divide-by-zero?))
]

A check can contain multiple assertions about the actual result. This is often
useful for error and predicate tests.

@examples[#:eval the-eval #:label #f
(test
  (check (modulo 5 0)
         #:error exn:fail:contract:divide-by-zero?
         #:error #rx"^modulo: "))
(test
  (check (range 10)
         #:with list?
         #:with (lambda (v) (= (length v) 10))))
]

Checks return @racket[(void)] by default, but they can optionally forward the
result of the actual expression, allowing the checked computation to be used in
other computations and other checks.

@examples[#:eval the-eval #:label #f
(test
  (define-values (n r)
    (check (quotient/remainder 10 3) #:values))
  (check (+ (* n 3) r) #:is 10))
]


@; ------------------------------------------------------------
@section[#:tag "api"]{Checkers API}

@defform[(test maybe-name maybe-loc def-or-expr ...)
         #:grammar
         ([maybe-name (code:line)
                      (code:line #:name name-expr)]
          [maybe-loc (code:line)
                     (code:line #:location loc-expr)
                     (code:line #:location-syntax loc-term)])
         #:contracts
         ([name-expr (or/c string? #f)]
          [loc-expr source-location?])]{

Evaluates the definitions and expressions as a test. If @racket[name-expr]
produces a string, it is used as the name of the test. If @racket[loc-expr] is
given, it is used as the test's location; otherwise the location is taken from
@racket[loc-term] or the @racket[test] expression itself. The result of the
@racket[test] expression is always @racket[(void)].

If a @racket[check] expression is executed during the evaluation of the test
body and fails, then evaluation of the test stops and the test is marked as
@emph{failed}. Otherwise, if evaluation of the test body completes, the test is
marked as @emph{passed}. Check failures are implemented by calling
@racket[raise] with special non-exception values. The @racket[test] form only
catches these values; it does not catch exceptions.

Tests may execute nested tests. Checks only affect the immediately enclosing
test; the failure of an inner nested test does not cause the outer test to fail.
}

@defform[(check actual-expr check-clause ... maybe-forward)
         #:grammar
         ([check-clause
           (code:line #:is expected-expr)
           (code:line #:is-not unexpected-expr)
           (code:line #:is-true)
           (code:line #:error predicate/regexp-expr)
           (code:line #:no-error)
           (code:line #:with predicate/checker-expr)]
          [maybe-forward
           (code:line)
           (code:line #:forward)
           (code:line #:values)])
         #:contracts
         ([predicate/regexp-expr (or/c (-> any/c any/c) regexp?)]
          [predicate/checker-expr (or/c (-> any/c any/c) checker?)])]{

A check evaluates @racket[actual-expr] and applies each @racket[check-clause] in
order to the result. If a clause indicates a problem with the result, execution
of the current test stops and the test is marked as @emph{failed}. Check failure
is signaled by raising an opaque non-exception value, so @racket[check] should not
be used outside of a test.

The result of @racket[actual-expr] may be a single value, multiple values, or a
raised exception (an instance of @racket[exn:fail] or a subtype). If
@racket[actual-expr] escapes through a continuation jump or by raising a value
that does not satisfy @racket[exn:fail?], its result is not caught by
@racket[check]. In particular, @racket[check] does not catch breaks
(@racket[exn:break]).

The following forms of @svar[check-clause] are supported:

@specsubform[(code:line #:is expected-expr)]{

Succeeds if the actual result is equal (@racket[equal?]) to the result of
@racket[expected-expr]. The evaluation of @racket[expected-expr] must produce a
single value or multiple values; exceptions in @racket[expected-expr] are not
caught. If @racket[expected-expr]'s result has multiple values, then
@racket[actual-expr]'s result must have the same number of values, and the
values must be pairwise @racket[equal?].

Equivalent to @racket[#:with (checker:equal expected-expr)].
}

@specsubform[(code:line #:is-not unexpected-expr)]{

Succeeds if the actual result has the same number of values as
@racket[unexpected-expr]'s result but is not equal (@racket[equal?]) to it.

Equivalent to @racket[#:with (checker:not-equal unexpected-expr)].
}

@specsubform[(code:line #:is-true)]{

Succeeds if the actual result is a single value that is not @racket[#f].

Equivalent to @racket[#:with (λ (v) v)], except for the information accompanying
a check failure.
}

@specsubform[(code:line #:error predicate/regexp-expr)]{

Succeeds if the actual expression raised an exception and that exception is
accepted by the given predicate or regular expression. If a predicate is given,
it is applied to the raised exception. If a regular expression is given, it is
used to check the exception's message (@racket[exn-message]).

Equivalent to @racket[#:with (checker:error predicate/regexp-expr)].
}

@specsubform[(code:line #:no-error)]{

Succeeds if the actual expression did not raise an exception---that is, it
produced a single value or multiple values.
}

@specsubform[(code:line #:with predicate/checker-expr)]{

If a predicate is given, the check succeeds if the actual expression's result is
a single value and the predicate accepts that value. (See
@racket[checker:predicate] for predicates over multiple values.)

If a checker is given, the checker is applied to the result. The kind of result
accepted (single value, multiple values, or raised exception) depends on the
checker.
}

If all check clauses succeed, the result of the @racket[check] expression is
determined by @racket[maybe-forward]. If @racket[maybe-forward] is absent, then
@racket[(void)] is returned. Otherwise, @racket[maybe-forward] must be one of
the following:

@specsubform[(code:line #:forward)]{

The @racket[check] expression produces the same result as
@racket[actual-expr]. That is, if @racket[actual-expr] produced values, the
@racket[check] expression returns those values; if @racket[actual-expr] raised
an exception, the @racket[check] expression re-raises that exception.
}

@specsubform[(code:line #:values)]{

If @racket[actual-expr] produced values, the values are returned; otherwise, a
check failure is signaled.

Equivalent to @racket[#:no-error #:forward].
}

}

@defproc[(checker? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a checker, @racket[#f] otherwise.
}

@; ----------------------------------------
@subsection[#:tag "api-checker"]{Constructing Checkers}

@deftogether[[
@defform[(checker:equal expected-expr)]
@defform[(checker:not-equal unexpected-expr)]
]]{

Returns checkers corresponding to @racket[check]'s @racket[#:is] and
@racket[#:is-not] clauses, respectively.
}

@defproc[(checker:predicate [predicate procedure?]
                            [#:arity-mask arity-mask exact-integer?
                                          (procedure-arity-mask predicate)]
                            [#:property property (or/c string? #f) #f])
         checker?]{

Returns a checker that accepts a result @racket[(values _v ...)] if
@racket[(predicate _v ...)] returns a true value. If @racket[arity-mask] is
given, then it is used to reject results having the wrong number of values
before @racket[predicate] is applied. (Note: @racket[arity-mask] is a bit-mask,
not a single arity; see @racket[procedure-arity-mask].) If @racket[property] is
a string, it is included in check failure information. It should describe the
meaning of the predicate.
}

@defproc[(checker:compare [compare (-> any/c any/c any/c)]
                          [compare-to any/c])
         checker?]{

Returns a checker that accepts a result if it is a single value @racket[_v] such
that @racket[(compare _v compare-to)] returns a true value.
}

@defproc[(checker:error [pred/rx (or/c (-> any/c any/c) regexp?)])
         checker?]{

Returns a checker that accepts a result if it represents @racket[(raise _v)] and
@racket[pred/rx] accepts @racket[_v]. Specifically, if @racket[pred/rx] is a
procedure, the check succeeds if @racket[(pred/rx _v)] returns a true value; if
@racket[pred/rx] is a regular expression, the check succeeds if
@racket[(regexp-match? pred/rx (exn-message _v))] returns true.
}

@; ----------------------------------------
@subsection[#:tag "api-run"]{Running Tests}

Tests are run automatically, and the default runner prints check failures to the
current error port and logs test results using @racket[test-log!]. The
@racket[run-tests] provides additional options.

@defproc[(run-tests [proc (-> any)]
                    [#:out out (or/c output-port? (-> output-port?))
                           (current-error-port)]
                    [#:progress? progress? boolean? #f]
                    [#:tell-raco? tell-raco? boolean? #t])
         void?]{

Calls @racket[(proc)] and reports any test failures by printing to
@racket[out]. When the procedure completes, a summary is printed to
@racket[out].

If @racket[progress?] is true and Racket is running in an interactive terminal,
then the procedure maintains a status line with the full name of the current
test and a count of passing and failing tests so far. If the terminal is not
available, @racket[progress?] has no effect.

If @racket[tell-raco?] is true, then each @racket[test] expression reports its
success or failure to @racketmodname[raco/testing] using @racket[test-log!].

@;{
If @racket[trace-level] is non-zero, then at the beginning of each test whose
nesting level is less than @racket[trace-level] (root tests are level 0) the
name of the test (or its location, if the test has no name) is printed, indented
at two spaces per level. If @racket[trace-level] is @racket[#t], it is treated
the same as @racket[+inf.0].
}

}

@; ------------------------------------------------------------
@section[#:tag "cmp-rackunit"]{Comparison with RackUnit and Others}

This library adopts a pure ``tests as actions'' model, unlike RackUnit, which
started with a ``tests as values'' model and then later mixed in partial ``tests
as actions'' support. This library does not distinguish test suites from test
cases. A test may execute @racket[check] expressions, acting as a test case, and
it may also execute nested @racket[test] expressions, acting as a test suite. If
a check occurs outside of any test, RackUnit automatically wraps it with an
anonymous test, but this library does not.

This library's @racket[test] form does not catch exceptions, so it does not
support RackUnit's ``test error'' status. Instead, this library makes
@racket[check] the sole form responsible for catching exceptions, as well as
handling multiple values. As a consequence, single-value checks, multiple-value
checks, and error checks all use the same @racket[check] interface, and check
failures due to exceptions are distinguished from test-scripting errors.

The design of this library was influenced by @tt{chk}, @tt{expectations}, and
@tt{test-more}, in addition to RackUnit.

@; ------------------------------------------------------------
@(close-eval the-eval)
