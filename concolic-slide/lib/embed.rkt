#lang racket/base

(require racket/contract

         racket/format

         txexpr)

(provide
 current-embed-tag
 current-embed-tag-regexp

 current-embedding
 embed-new-value!
 embed-id-ref

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

;; TODO FIXME: memory leak
(define current-embed-tag-hash (make-hash))
(define (current-embed-tag-regexp)
  (define tag (current-embed-tag))
  (hash-ref! current-embed-tag-hash
             (current-embed-tag)
             (λ ()
               (regexp (string-append (regexp-quote (current-embed-tag)) "[0-9]+ff")))))

(define/contract current-embedding
  (parameter/c (hash/c exact-nonnegative-integer? txexpr?))
  (make-parameter (make-hash)))

(define (embed-id-ref embedding tag)
  (hash-ref embedding
            (string->number
             (substring tag
                        (string-length (current-embed-tag))
                        (- (string-length tag) 2)))))

(define embedding-cache (make-hash))
(define (embed-new-value! val [try-cache? #f])
  (define h (current-embedding))
  (cond
    [(and try-cache? (hash-has-key? embedding-cache val))
     (hash-ref embedding-cache val)]
    [else
     (define tag-id (hash-count h))
     (define tag (format "~a~aff" (current-embed-tag) tag-id))
     (hash-set! h tag-id val)
     (when try-cache?
       (hash-set! embedding-cache val tag))
     tag]))

(define (can-cache? tx)
  (and (null? (get-attrs tx)) (get-elements tx)))

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
       (embed-new-value! new-txexpr (can-cache? new-txexpr)))
     embed-id]
    [else (error 'pollen-setup "unknown value ~s" x)]))

;; string to txexpr-elements
(define (to-txexpr-elements/unembed-txexpr s)
  (define embedding (current-embedding))
  (define p (current-embed-tag-regexp))
  (define all-tags
    (regexp-match-positions* p s))
  (let loop ([next-tag all-tags]
             [start-pos 0])
    (cond
      [(pair? next-tag)
       (define match-interval (car next-tag))
       (define tag (substring s (car match-interval) (cdr match-interval)))
       (define prefix
         (cond [(= start-pos (car match-interval)) '()]
               [else (list (substring s start-pos (car match-interval)))]))
       (append prefix
               (list (unembed-txexpr (embed-id-ref embedding tag) tag))
               (loop (cdr next-tag) (cdr match-interval)))]
      [else
       (if (< start-pos (string-length s))
           (list (substring s start-pos))
           '())])))

;; unembed tags; txexpr to txexpr
(define unembed-cache (make-hash))
(define (unembed-txexpr x [tag #f])
  (define elements
    (get-elements x))
  (cond
    [(and tag (hash-has-key? unembed-cache tag))
     (hash-ref unembed-cache tag)]
    [else
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
     (define tx
       (txexpr (get-tag x)
               (get-attrs x)
               unembedded-elements))
     (when tag
       (hash-set! unembed-cache tag tx))
     tx]))
