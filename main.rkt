#lang racket/base
(require (for-syntax racket/base))
(provide (all-defined-out))

(define-syntax (define-data stx)
  (syntax-case stx (lib operation abstraction)
      ((_ name
          (lib init (primitive ...))
          (operation (op-name op-body) ...)
          (abstraction (ab-name ab-body) ...))
       (with-syntax (((op-alias ...) (generate-temporaries #'(op-name ...))))
         #'(module name racket/base
             (module represent init
               (require primitive ...)
               (provide op-alias ...)
               (define op-alias op-body)
               ...)
             (module abstract init
               (require (rename-in (submod ".." represent)
                                   (op-alias op-name)
                                   ...))
               (provide (all-from-out (submod ".." represent)) ab-name ...)
               (define ab-name ab-body)
               ...)
             (require 'abstract)
             (provide (all-from-out 'abstract)))))))
