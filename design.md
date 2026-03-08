# Design goals

- simplicity
  - tests are actions, not values
    - removes need for fixtures, etc
  - conceptual simplicity
    - checks are not tests (even at top level)
    - `only` check catches exceptions, only exn:fail
- uniform interface
  - `check` rather than `check-equals?`+`check-pred`+`check-exn`+...
- terse, flexible syntax
  - check clauses abbreviate common patterns; checkers more general
  - try to preserve left-to-right evaluation order
  - take advantage of keywords, optional clauses


# Feature plans and anti-plans

- change from "actual/expected" to "got/wanted"?

## Test

- skip -- probably not, use let/ec
- fixtures -- no
- xfail -- no
  - use tag; don't complicate core
  - maybe have xfail in "library of interpretations"
- tags, other info
  - useful for external analysis/interpretation of test results
  - need to decide what is allowed, what can affect test behavior
  - declare vs dynamically set
- stable identifiers
  - support external analysis of tests
  - maybe support name with "${key}" interpolation,
    then uninterpolated name acts as stable identifier
- add option to catch exceptions (`#:catch`?)
  - add "test-error" status, or treat as if check failure?
  - maybe unnecessary with check `#:forward` mode

## Check

- fixtures -- probably not
  - eg, `reset-atomic`, `reset-parameters`
- more checker clauses?
  - `#:satisfies pred` -- eh, not needed for arity=1, awkward to add optional arity
  - `#:is-any-value` -- eh, probably ok with `#:no-error`, or be more specific
  - `#:match pattern` negation, multi-value support -- no, below threshold
- could make `(check expr)` implicitly `#:no-error` -- no, irregular

## Checkers

- expose checker interface -- yes, but not soon
- `checker:regexp`

## UI

- option to bail after N failures -- maybe
- terminal ui
  - print currently executing test
  - print running status as "pass:fail", "pass:skip:fail"
- port rackunit gui


# Meaning and interpretation

A test suite is a collection of propositions about the behavior of a program.
How do we decide what propositions to test? What interpretation do we use to
decide whether "all's good" or "attention required ... here"?

- expected to fail (XFAIL)
  - known bug / wontfix
  - not yet implemented; due in later milestone
- nondeterministic
  - could be "network is down" rather than "software is broken"
  - could be probabilistic; then might still want to apply statistical constraints
    - like "fewer than 10% of independent prob. tests failed"
- importance
  - critical functionality vs incidental changes
    - eg, if UI changes, human-review, then update test?
  - legacy interfaces: tests catch breakage, either fix or announce incompatibility

Implementing interpretations
- meta-tests: propositions about results of a test suite
- filter / view on test results
  - eg, mark PASS with "green check", FAIL with "red X", XFAIL with "green X"
- combination
