#lang racket/base

(require racket/format
         racket/match

         txexpr)

(provide (all-defined-out)
         (all-from-out racket/format))

(define (@when guard . content)
  (cond [guard (txexpr '@ empty content)]
        [else  (txexpr '@ empty '())]))

(define frame-counter 0)
(define (get-frame-counter-name)
  (format "frame~a" frame-counter))
(define (frame #:option [option/#f #f] . content)
  (set! frame-counter (+ 1 frame-counter))
  (txexpr 'frame
          (cond
            [option/#f (list (list 'option option/#f))]
            [else      empty])
          content))

;; https://tex.stackexchange.com/questions/24186/
;; how-to-center-text-without-adding-space-and-not-
;; altering-alignment-of-surroundin
(define (tightcenter . elements)
  (txexpr '@ empty
          (append
           (list "{"
                 "\\setlength{\\topsep}{0pt}"
                 "\\setlength{\\parskip}{0pt}\n"
                 (txexpr 'center empty elements)
                 "}"))))

(define (thuge . elements)
  (txexpr 'text empty
          (cons (txexpr 'huge empty '())
                elements)))

;; texts
(define sp (txexpr 'sp '() '()))
(define Sp (txexpr 'Sp '() '()))
(define SP (txexpr 'SP '() '()))
(define unsp (txexpr 'unsp '() '()))

(define (braced . elements)
  (txexpr '@ empty
          (append (list (txexpr 'lbrace empty '()))
                  elements
                  (list (txexpr 'rbrace empty '())))))

(define verbatim-command-chars #<<CHARS
\\\{\}
CHARS
  )

(define verbatim-math-codes #<<CODES
{\catcode`$=3\catcode`^=7\catcode`_=8}
CODES
  )

(define tlbr (txexpr 'textlbrace empty '()))
(define trbr (txexpr 'textrbrace empty '()))

(define (lbraced . elements)
  (txexpr '@ empty
          (append (list tlbr)
                  elements
                  (list trbr))))

;; math definitions
(define Ga (txexpr 'alpha))
(define Gb (txexpr 'beta))
(define GL (txexpr 'Lambda))
(define Gl (txexpr 'lambda))
(define GS (txexpr 'Sigma))
(define Gs (txexpr 'sigma))
(define GG (txexpr 'Gamma))
(define Gg (txexpr 'gamma))
(define GD (txexpr 'Delta))
(define Gd (txexpr 'delta))
(define Gt (txexpr 'tau))
(define Gr (txexpr 'rho))
(define Gm (txexpr 'mu))
(define GP (txexpr 'Pi))
(define Gp (txexpr 'pi))

(define tteq    (txexpr 'tteq))
(define ttplus  (txexpr 'ttplus))
(define tttimes (txexpr 'tttimes))

(define star   (txexpr 'star empty '()))
(define forall (txexpr 'forall empty '()))
(define exists (txexpr 'exists empty '()))
(define entail (txexpr 'vdash empty '()))
(define (to)   (txexpr 'to empty '()))
(define (quad)   (txexpr 'quad empty '()))
(define (cdot)   (txexpr 'cdot empty '()))

(define grammar-rule
  "::=")

(define grammar-sep
  (txexpr '@ empty
          (list SP "|" SP)))

(define grammar-sepr
  (txexpr '@ empty
          (list SP "|")))

(define bbN
  (txexpr 'mathbb empty '("N")))

(define bbZ
  (txexpr 'mathbb empty '("Z")))

(define bbB
  (txexpr 'mathbb empty '("B")))

(define (fn args body)
  (txexpr '@ empty
          (list Gl args "." sp body)))

(define (fnp args body)
  (txexpr '@ empty
          (list "(" (fn args body) ")")))

(define (lmax . elements)
  (txexpr '@ empty
          (cons
           (txexpr 'max empty '())
           elements)))

(define (lmin . elements)
  (txexpr '@ empty
          (cons
           (txexpr 'min empty '())
           elements)))

(define lnot
  (txexpr 'not empty '()))

(define emptyset
  (txexpr 'emptyset empty '()))

(define (lset . elements)
  (txexpr '@ empty
          (append (list (txexpr 'textlbrace empty '()))
                  elements
                  (list (txexpr 'textrbrace empty '())))))

(define llog
  (txexpr 'log empty '()))

(define (dom . elements)
  (txexpr '@ empty
          (append (list (txexpr 'text empty '("dom(")))
                  elements
                  (list ")"))))

(define (rule name)
  (txexpr 'textsc empty (list (format "[~a]" name))))

(define (:-types ctxt term type)
  (txexpr '@ empty
          (list ctxt (txexpr 'vdash empty '()) term ":" type)))

(define sum
  (txexpr 'sum empty '()))

(define int
  (txexpr 'int empty '()))

(define prod
  (txexpr 'prod empty '()))

(define (frac num denom)
  (txexpr 'frac empty
          (list num
                (txexpr 'rbrace empty '())
                (txexpr 'lbrace empty '())
                denom)))

(define (dfrac num denom)
  (txexpr 'dfrac empty
          (list num
                (txexpr 'rbrace empty '())
                (txexpr 'lbrace empty '())
                denom)))

;; infer
;;   (infer) => \AxiomC{}
;;   (infer axm ...) => \AxiomC{axm ...}
;;   (infer premise ...* #:----- conclusion) => premise ... \xxxInfC{conclusion}
(define (infer #:label [label/#f #f]
               #:----- [conclusion/#f #f]
               . elements)
  (cond
    [(not conclusion/#f)
     (txexpr 'AxiomC empty elements)]
    [else
     (define len (length elements))
     (define tag
       (match len
         [(or 0 1) 'UnaryInfC]
         [2        'BinaryInfC]
         [3        'TrinaryInfC]))
     (txexpr '@ empty
             (append (if (null? elements)
                         (list (txexpr 'AxiomC empty '()))
                         elements)
                     (if label/#f
                         (list (txexpr 'RightLabel empty (list label/#f)))
                         '())
                     (list (txexpr tag empty (list conclusion/#f)))))]))

(define (infer$ #:label [label/#f #f]
                #:----- [conclusion/#f #f]
                . elements)
  (cond
    [(not conclusion/#f)
     (infer
      (txexpr '$ empty elements)
      #:label label/#f
      #:----- conclusion/#f)]
    [else
     (apply infer
            elements
            #:label label/#f
            #:----- (txexpr '$ empty (list conclusion/#f)))]))

(define (intersperse sep xs)
  (for*/list ([(i x) (in-parallel (in-naturals) (in-list xs))]
              [is-sep? (in-list '(#t #f))]
              #:when (not (and (zero? i) is-sep?)))
    (if is-sep? sep x)))

(define mredex (txexpr 'mathbf empty '("r")))
(define mreduce (txexpr 'to empty '()))
(define mreduce*
  (txexpr '@ empty
          (list mreduce "^*")))
