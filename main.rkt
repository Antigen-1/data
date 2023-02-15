#lang racket/base
(require (for-syntax racket/base))
(provide (all-defined-out))

(define-syntax (define-data stx)
  (syntax-case stx (lib representation abstraction)
      ((_ name
          (lib primitive ...)
          (representation (rp-name rp-body) ...)
          (abstraction (ab-name ab-body) ...))
       #'(begin
           (define-values (rp-name ...) (let () (local-require primitive ...) (values rp-body ...)))
           (define-values (ab-name ...) (values ab-body ...))
           (provide (for-space #f rp-name ... ab-name ...)
                    (for-space name (for-space representation rp-name ...) (for-space abstraction ab-name ...)))
           ))))
