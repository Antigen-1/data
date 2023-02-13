#lang racket/base
(require (for-syntax racket/base))
(provide (all-defined-out))

(define-syntax-rule
  (define-data
    name
    (share ...)
    (primitive ...)
    ((operation op) ...)
    ((abstraction ab) ...))
  (module name racket/base
    (module represent racket/base
      (require share ... primitive ...)
      (provide operation ...)
      (define operation op)
      ...)
    (module abstract racket/base
      (require (submod ".." represent) share ...)
      (provide abstraction ...)
      (define abstraction ab)
      ...)
    (require 'represent 'abstract)
    (provide (all-from-out 'represent 'abstract))))
