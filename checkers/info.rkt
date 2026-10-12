;; Copyright 2026 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang info

;; pkg info

(define collection "checkers")
(define blurb "A simple testing framework (documentation only)")
(define deps '("base" "compiler-lib" "checkers-lib"))
(define build-deps
  '("scribble-lib"
    "racket-doc"))
(define implies '("checkers-lib"))
(define pkg-authors '(ryanc))
(define license 'Apache-2.0)

;; collection info

(define name "checkers")
(define scribblings '(["checkers.scrbl" ()]))
