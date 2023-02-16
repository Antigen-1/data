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
              (tmp-id (string->symbol (symbol->string (gensym "tmp"))))
              (add-tmp (lambda (stx) ((make-interned-syntax-introducer tmp-id) stx 'add)))
              (add-rp (lambda (stx) ((make-interned-syntax-introducer rp-id) stx 'add)))
              (add-ab (lambda (stx) ((make-interned-syntax-introducer ab-id) stx 'add))))
         (with-syntax (((rp-space-name ab-space-name tmp-space-name) (map (lambda (sym) (datum->syntax #'stx sym)) (list rp-id ab-id tmp-id)))
                       ((rp-name ... rp-body ...) (map (compose add-rp add-tmp) (syntax->list #'(rp-name ... rp-body ...))))
                       ((ab-name ... ab-body ...) (map (compose add-ab add-rp) (syntax->list #'(ab-name ... ab-body ...)))))
          #'(begin
              (require (for-space tmp-space-name primitive ...))
              (define rp-name rp-body) ...
              (define ab-name ab-body) ...
              (provide (for-space rp-space-name rp-name ...)
                       (for-space ab-space-name ab-name ...)
                       (for-space #f rp-name ... ab-name ...))
              ))))))
