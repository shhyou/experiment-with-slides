#lang racket/base

(require racket/match

         racket/hash

         racket/pretty

         txexpr
         (only-in markdown parse-markdown)

         "embed.rkt")

(provide
 DEBUG-ENRICH?
 enrich-with-markdown)

(define DEBUG-ENRICH? (make-parameter #f))

(define (enrich-with-markdown tx)
  (match-define (list tx-id embedding)
    (parameterize ([current-embedding (make-hash)])
      (define tx-id
        (to-string/embed-txexpr tx))
      (list tx-id (current-embedding))))

  (define verbatim-embedding
    (for/hash ([(embed-id x) (in-hash embedding)]
               #:when
               (string-ci=? "true"
                            (attr-ref (get-attrs x)
                                      'verbatim
                                      (λ () "false"))))
      (values embed-id x)))

  (define enriched-embedding
    (for/hash ([(embed-id x) (in-hash embedding)]
               #:when
               (not
                (string-ci=? "true"
                             (attr-ref (get-attrs x)
                                       'verbatim
                                       (λ () "false")))))
      (when (DEBUG-ENRICH?)
        (printf "~a:~a\n" embed-id (get-tag x))
        (printf "  from: ~s\n" (get-elements x)))
      (define all-str
        (apply string-append (get-elements x)))
      (define enriched-elements
        (parse-markdown all-str))
      (when (DEBUG-ENRICH?)
        (printf "  to  : ~s\n" enriched-elements))
      (values embed-id
              (txexpr (get-tag x) (get-attrs x) enriched-elements))))

  (when (DEBUG-ENRICH?)
    (port-count-lines! (current-output-port))
    (pretty-write embedding)
    (printf "PARSED:\n    ")
    (pretty-write enriched-embedding))

  (define full-embedding
    (hash-union verbatim-embedding enriched-embedding))

  (define enriched-unembedded-tx
    (parameterize ([current-embedding full-embedding])
      (unembed-txexpr (hash-ref enriched-embedding tx-id))))

  (when (DEBUG-ENRICH?)
    (printf "FINAL:\n    ")
    (pretty-write enriched-unembedded-tx))

  enriched-unembedded-tx)
