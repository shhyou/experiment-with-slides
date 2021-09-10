#lang racket/base

(require racket/match

         racket/format
         racket/pretty

         txexpr

         "lib/txexpr.rkt"
         "lib/markdown.rkt"
         "lib/tex-backend.rkt")

(provide (except-out
          (all-defined-out)
          user-date)
         (rename-out
          [user-date date])
         ->tex-string)

(module setup racket/base
  (require (for-template "lib/tex-top.rkt"))

  (provide allow-unbound-ids?)
  (define allow-unbound-ids? #'tex-top)
  )

(define (raw-latex . elements)
  (txexpr 'raw-latex '((verbatim "true")) elements))

;; user tags

(define br
  (txexpr 'br empty '()))

(define (item . elements)
  (txexpr 'li empty elements))

(define (items xs)
  (txexpr 'ul empty xs))

(define (enumerate xs)
  (txexpr 'ol empty xs))

(define ($ . elements)
  (txexpr '$ empty elements))

(define ($$ . elements)
  (txexpr '$$ empty elements))

(define (user-date . elements)
  (txexpr 'date empty elements))

(define (href #:param link . elements)
  (txexpr 'a (list (list "href" link))
          elements))

(define (url link)
  (href #:param link link))

;; root element, enriching string with markdown parsing
(define (root . elements)
  (unless (txexpr-elements? elements)
    (define failing-positions
      (txexpr-elements-locate-failing-positions elements))
    (define failing-elements
      (for/list ([i (in-range 5)]
                 [position (in-list (reverse failing-positions))])
        (~s (list-ref position 1)
            #:limit-marker "..." #:max-width 60)))
    (define formats
      (build-list (length failing-elements)
                  (λ (i) "\n in: ~a")))
    (apply error
           'root
           (apply string-append "not an txexpr-element?:" formats)
           failing-elements))
  (enrich-with-markdown
   (txexpr 'root empty elements)))
