#lang racket/base

(require (for-syntax racket/base racket/syntax racket/string)

         (only-in txexpr [txexpr tex-wrapper-txexpr])
         pollen/tag)

(provide tex-top
         (for-syntax tex-top-wrapper))

(define-syntax (tex-top stx)
  (syntax-case stx ()
    [(_ . ID)
     (tex-top-wrapper #'ID)]))

(define-for-syntax (tex-top-wrapper stx)
  (define id (symbol->string (syntax-e stx)))
  (define has-underscore-or-caret?
    (for/first ([i (in-naturals)]
                [c (in-string id)]
                #:when (or (char=? #\_ c) (char=? #\^ c) (char=? #\. c)))
      i))
  (cond
    [(and has-underscore-or-caret? (< 0 has-underscore-or-caret?))
     (define prefix
       (format-id stx "~a" (substring id 0 has-underscore-or-caret?)))
     (define suffix
       (substring id has-underscore-or-caret?))
     #`(tex-wrapper-txexpr '@ '()
                           (list (def/c . #,prefix)
                                 '#,suffix))]
    [else
     #`(default-tag-function '#,stx)]))

(define-syntax (def/c stx)
  (syntax-case stx ()
    [(_ . ID)
     (identifier-binding #'ID)
     #'ID]
    [(_ . ID)
     #'(#%top . ID)]))
