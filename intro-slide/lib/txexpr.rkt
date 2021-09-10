#lang racket/base

(require txexpr)

(provide
 txexpr-elements-locate-failing-positions)

(define (txexpr-elements-locate-failing-positions elements)
  (define failing-element/#f
    (for/first ([i (in-naturals)]
                [element (in-list elements)]
                #:when (not (txexpr-element? element)))
      (list i element)))
  (define element
    (and failing-element/#f (list-ref failing-element/#f 1)))
  (cond
    [(not failing-element/#f) '()]
    [(and (list? element)
          (>= (length element) 3)
          (symbol? (car element))
          (list? (cadr element)))
     (cons failing-element/#f
           (txexpr-elements-locate-failing-positions (cddr element)))]
    [else
     (list failing-element/#f)]))
