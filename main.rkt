#lang racket/base
(require (for-syntax racket/base) racket/splicing)
(provide (all-defined-out))

(define-syntax (define-data stx)
  (syntax-case stx (lib representation abstraction)
      ((_ name
          (lib primitive ...)
          (representation (rp-name rp-body) ...)
          (abstraction (ab-name ab-body) ...))
       (let* ((make-id (lambda (str) (string->symbol (format "~a:~a" (syntax->datum #'name) str))))
              (rp-id (make-id "representation"))
              (ab-id (make-id "abstraction"))
              (add-rp (lambda (stx) ((make-interned-syntax-introducer rp-id) stx 'add)))
              (add-ab (lambda (stx) ((make-interned-syntax-introducer ab-id) stx 'add))))
         ((compose add-rp add-ab)
          #`(begin
              (splicing-let () (local-require primitive ...) (define rp-name rp-body) ...)
              (splicing-let () (define ab-name ab-body) ...)
              (provide (for-space #,rp-id rp-name ...)
                       (for-space #,ab-id ab-name ...)
                       (for-space #f rp-name ... ab-name ...))
              ))))))
