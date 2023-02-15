#lang racket/base
(require (for-syntax racket/base))
(provide (all-defined-out))

(define-syntax (define-data stx)
  (syntax-case stx (lib representation abstraction)
      ((_ name
          (lib primitive ...)
          (representation (rp-name rp-body) ...)
          (abstraction (ab-name ab-body) ...))
       (let ((add-nm (lambda (stx) ((make-interned-syntax-introducer (syntax->datum #'name)) stx 'add)))
             (add-rp (lambda (stx) ((make-interned-syntax-introducer 'representation) stx 'add)))
             (add-ab (lambda (stx) ((make-interned-syntax-introducer 'abstraction) stx 'add))))
         (with-syntax (((rp-name ...) (map (compose add-rp add-nm) (syntax->list #'(rp-name ...))))
                       ((rp-body ...) (map (compose add-rp add-nm) (syntax->list #'(rp-body ...))))
                       ((ab-name ...) (map (compose add-ab add-nm) (syntax->list #'(ab-name ...))))
                       ((ab-body ...) (map (compose add-ab add-rp add-nm) (syntax->list #'(ab-body ...)))))
           #'(begin
               (define-values (rp-name ...) (let () (local-require primitive ...) (values rp-body ...)))
               (define-values (ab-name ...) (values ab-body ...))
               (provide (for-space #f rp-name ... ab-name ...)
                        (for-space name (for-space representation rp-name ...) (for-space abstraction ab-name ...)))
               ))))))
