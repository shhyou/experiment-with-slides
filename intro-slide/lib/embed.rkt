#lang racket/base

(require racket/contract

         racket/format

         txexpr)

(provide
 current-embed-tag
 current-embed-tag-regexp

 current-embedding
 embed-new-value!

 to-string/embed-txexpr
 to-txexpr-elements/unembed-txexpr
 unembed-txexpr)

;; tag embedding library
(define current-embed-tag
  (make-parameter
   (apply string-append
          "◇pollen"
          (for/list ([i (in-range 3)])
            (~r (random (* 36 36))
                #:base '(up 36)
                #:min-width 2
                #:pad-string "0")))))

(define (current-embed-tag-regexp)
  (string-append (regexp-quote (current-embed-tag)) "[0-9]+ff"))

(define/contract current-embedding
  (parameter/c (hash/c string? txexpr?))
  (make-parameter (make-hash)))

(define (embed-new-value! val)
  (define h (current-embedding))
  (define tag (format "~a~aff" (current-embed-tag) (hash-count h)))
  (hash-set! h tag val)
  tag)

;; modified from pollen/private/to-string
(define (to-string/embed-txexpr x)
  (cond
    [(string? x) x]
    [(or (null? x) (void? x)) ""]
    [(or (symbol? x) (number? x) (path? x) (char? x)) (format "~a" x)]
    [(txexpr? x)
     (define new-elements
       (for/list ([e (in-list (get-elements x))])
         (to-string/embed-txexpr e)))
     (define new-txexpr
       (txexpr (get-tag x) (get-attrs x) new-elements))
     (define embed-id
       (embed-new-value! new-txexpr))
     embed-id]
    [else (error 'pollen-setup "unknown value ~s" x)]))

;; string to txexpr-elements
(define (to-txexpr-elements/unembed-txexpr s)
  (define embedding (current-embedding))
  (define p (current-embed-tag-regexp))
  (let loop ([start-pos 0])
    (define next-tag
      (regexp-match-positions p s start-pos))
    (cond
      [next-tag
       (define match-interval (car next-tag))
       (define tag (substring s (car match-interval) (cdr match-interval)))
       (define prefix
         (cond [(= start-pos (car match-interval)) '()]
               [else (list (substring s start-pos (car match-interval)))]))
       (append prefix
               (list (unembed-txexpr (hash-ref embedding tag)))
               (loop (cdr match-interval)))]
      [else
       (if (< start-pos (string-length s))
           (list (substring s start-pos))
           '())])))

;; unembed tags; txexpr to txexpr
(define (unembed-txexpr x)
  (define elements
    (get-elements x))
  (define unembedded-elements
    (apply
     append
     (for/list ([sub-x (in-list elements)])
       (cond
         [(list? sub-x)
          (list (unembed-txexpr sub-x))]
         [(string? sub-x)
          (to-txexpr-elements/unembed-txexpr sub-x)]
         [else (list sub-x)]))))
  (txexpr (get-tag x)
          (get-attrs x)
          unembedded-elements))
