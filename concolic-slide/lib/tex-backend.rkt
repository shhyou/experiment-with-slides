#lang racket/base

(require racket/class
         racket/match

         racket/string
         racket/port

         txexpr)

(provide
 (struct-out TX)
 tex
 environment-tex-commands
 ->tex-tree
 ->tex-string

 no-param-tex-commands
 is-no-param-tex-command?
 pretty-print-newline-tex-commands
 is-pretty-print-newline-tex-command?
 environment-tex-commands
 is-environment-tex-command?
 )

;; tex backend formatting

(struct TX (tag option params content)
  #:transparent)

(define (tex tag content
             #:option [option #f]
             #:params [params #f])
  (TX tag option params content))

(define DEFAULT-NO-PARAM-TEX-COMMANDS
  (list
   'times 'bmod
   'int 'sum 'prod
   'cup 'bigcup 'cap 'bigcap
   'not 'neg
   'le 'leq 'ge 'geq 'ne 'neq 'equiv
   'max 'min 'sin 'cos 'tan 'cot 'log 'exp
   'lfloor 'rfloor 'lceil 'rceil
   'dots 'cdots 'ldots 'vdots 'ddots
   'in 'ni
   'subset 'subseteq 'superset 'superseteq 'emptyset
   'forall 'exists 'land 'lor 'wedge 'vee
   'to 'mapsto 'leadsto
   'rightarrow 'Rightarrow 'leftarrow 'Leftarrow
   'longrightarrow 'Longrightarrow 'longleftarrow 'Longleftarrow
   'hookrightarrow
   'node
   'topmargin
   'newblock
   ))

(define no-param-tex-commands
  (make-parameter DEFAULT-NO-PARAM-TEX-COMMANDS))

(define (is-no-param-tex-command? tag)
  (member tag (no-param-tex-commands)))

(define DEFAULT-PRETTY-PRINT-NEWLINE-TEX-COMMANDS
  (list
   'author 'date 'title

   'h1 'h2

   'item
   'frametitle

   'AxiomC
   'UnaryInfC
   'BinaryInfC
   'TrinaryInfC
   'LeftLabel
   'RightLabel
   ))

(define pretty-print-newline-tex-commands
  (make-parameter DEFAULT-PRETTY-PRINT-NEWLINE-TEX-COMMANDS))

(define (is-pretty-print-newline-tex-command? tag)
  (member tag (pretty-print-newline-tex-commands)))

(define DEFAULT-ENVIRONMENT-TEX-COMMANDS
  (list
   'document
   'minipage

   'center
   'verbatim
   'Verbatim
   'quote

   'enumerate
   'itemize

   'equation 'equation* 'aligned 'flalign 'flalign*
   'array
   'prooftree

   'figure

   'tikzpicture

   'lstlisting

   'thebibliography

   'frame
   'block
   'alertblock
   'definition
   'example
   'theorem
   'corollary
   'proof
   ))

(define environment-tex-commands
  (make-parameter DEFAULT-ENVIRONMENT-TEX-COMMANDS))

(define (is-environment-tex-command? tag)
  (member tag (environment-tex-commands)))

(define (hash-remove/ignore h key)
  (if (hash-has-key? h key)
      (hash-remove h key)
      h))

(define ->tex-tree%
  (class object%
    [init-field [(root-x root)]]

    (define/public (->tree)
      (unless (equal? 'root (get-tag root-x))
        (error '->tex-tree%::->tree "expecting a txexpr root but got ~s" root-x))
      (txelements->forest (get-elements root-x)))

    (define/private (txelements->forest xs)
      (define xs/without-leading-p
        (match xs
          [(cons (txexpr 'p '() elements)
                 (and rest (cons _ _)))
           (append elements rest)]
          [_ xs]))
      (for/list ([x (in-list xs/without-leading-p)])
        (match x
          [(? string?) x]
          [(or (? txexpr?) (? symbol?)) (txexpr->tree x)])))

    (define/private (remove-beginning-p x)
      (match x
        [(txexpr tag attr
                 (list (txexpr 'p '() elements)))
         (txexpr tag attr elements)]
        [_ x]))

    (define/private (txexpr->tree x)
      (match (remove-beginning-p x)
        ['lsquo "`"]
        ['rsquo "'"]
        ['ldquo "``"]
        ['rdquo "\""]
        ['mdash "--"]
        ['ndash "-"]
        ['hellip "..."]
        [(txexpr 'br '() '())
         (tex '\\ '())]
        [(txexpr 'h1 _ elements)
         (tex 'section (txelements->forest elements))]
        [(txexpr 'h2 _ elements)
         (tex 'subsection (txelements->forest elements))]
        [(txexpr 'sp '() '()) "\\,"]
        [(txexpr 'Sp '() '()) "\\:"]
        [(txexpr 'SP '() '()) "\\;"]
        [(txexpr 'unsp '() '()) "\\!"]
        [(txexpr 'lbrace '() '()) "{"]
        [(txexpr 'rbrace '() '()) "}"]
        [(txexpr 'textlbrace '() '()) "\\{"]
        [(txexpr 'textrbrace '() '()) "\\}"]
        [(txexpr 'raw-latex _ elements)
         elements]

        [(txexpr 'strong '() elements)
         (tex 'textbf (txelements->forest elements))]
        [(txexpr 'em '() elements)
         (tex 'emph (txelements->forest elements))]
        [(txexpr 'blockquote '() elements)
         (list "\n" (tex 'quote (txelements->forest elements)))]
        [(txexpr 'code '() elements)
         (tex 'texttt (txelements->forest elements))]
        [(txexpr 'pre '() (list (txexpr 'code '() elements)))
         (tex 'verbatim (txelements->forest elements))]

        [(txexpr 'ul '() elements)
         (tex 'itemize (txelements->forest elements))]
        [(txexpr 'ol '() elements)
         (tex 'enumerate (txelements->forest elements))]
        [(txexpr 'li '() elements)
         (tex 'item (txelements->forest elements))]

        [(txexpr 'p '() elements)
         (cons "\n\n" (txelements->forest elements))]

        [(txexpr '$ '() (and elements (not '())))
         (list
          "$"
          (txelements->forest elements)
          "$")]
        [(txexpr '$$ '() (and elements (not '())))
         (list
          "\\[\n"
          (txelements->forest elements)
          "\\]\n")]

        [(txexpr 'a (list (list 'href (? string? link))) elements)
         (tex 'href #:params (list link) (txelements->forest elements))]

        [(txexpr 'bigparens
                 (list-no-order
                  (list 'left lopen)
                  (list 'right rclose))
                 elements)
         (list
          (format "\\left~a" lopen)
          (txelements->forest elements)
          (format "\\right~a" rclose))]
        [(txexpr 'selective
                 (app attrs->hash attrs)
                 (list
                  (txexpr (and list-tag (or 'ol 'ul))
                          list-attrs
                          list-contents)))
         (define (split-number-list/#f comma-separated-str-list)
           (cond
             [(not comma-separated-str-list) #f]
             [else
              (define str-list
                (string-split comma-separated-str-list ","))
              (for/list ([str (in-list str-list)])
                (string->number str 10 'read))]))
         (define filter-list
           (split-number-list/#f
            (hash-ref attrs 'filter (λ () #f))))
         (define faded-list
           (split-number-list/#f
            (hash-ref attrs 'faded (λ () #f))))
         (define faded-color
           (hash-ref attrs ''fadedcolor (λ () "lightgray")))
         (define filtered-contents
           (filter
            values
            (for/list ([i (in-naturals)]
                       [item (in-list list-contents)])
              (cond
                [(or (not filter-list)
                     (member i filter-list))
                 (cond
                   [(and faded-list (member i faded-list))
                    (match item
                      [(txexpr 'li item-attrs item-contents)
                       (define faded-contents
                         `(,(format "{\\color{~a}" faded-color)
                           ,@item-contents
                           "}"))
                       (txexpr
                        'li
                        item-attrs
                        faded-contents)]
                      [_ item])]
                   [else
                    item])]
                [else
                 #f]))))
         (list
          (txexpr->tree
           (txexpr list-tag
                   list-attrs
                   filtered-contents)))]
        [(and x (txexpr tag-fallthrough (app attrs->hash attrs) elements))
         (define option/#f (hash-ref attrs 'option (λ () #f)))
         (define param/#f (hash-ref attrs 'param (λ () #f)))

         (define attrs-without-option
           (for/fold ([attrs attrs])
                     ([key (in-list '(option param verbatim))])
             (hash-remove/ignore attrs key)))
         (define joined-option
           (string-join
            (for/list ([(key value) (in-hash attrs-without-option)])
              (define key-str (symbol->string key))
              (if (non-empty-string? key-str)
                  (string-append key-str "=" value)
                  value))
            ","))
         (define params/#f (if param/#f (list param/#f) #f))

         (when (and option/#f (not (hash-empty? attrs-without-option)))
           (error '->tex-tree%::txexpr->tree
                  "txexpr have both #:option and non-#:option attributes: ~s"
                  x))
         (tex tag-fallthrough (txelements->forest elements)
              #:option (or option/#f
                           (and (not (hash-empty? attrs-without-option))
                                joined-option))
              #:params params/#f)]))

    (super-new)
    ))

(define (->tex-tree root)
  (send (new ->tex-tree% [root root])
        ->tree))

(define (->tex-string root)
  (define tree (->tex-tree root))
  (with-output-to-string
    (λ ()
      (display-tex-tree tree)
      (newline))))

(define (display-tex-tree tree)
  (cond
    [(string? tree)
     (write-string tree)]
    [(list? tree)
     (for ([element (in-list tree)])
       (display-tex-tree element))]
    [(TX? tree)
     (match-define (struct* TX ([tag tag]
                                [option option]
                                [params params]
                                [content content]))
       tree)
     (cond
       [(and (equal? tag '\\) (null? content))
        (printf " \\\\\n")]
       [(not (is-environment-tex-command? tag))
        (printf "\\~a" tag)
        (cond
          [(is-no-param-tex-command? tag)
           (unless (and (not option) (not params) (null? content))
             (error 'display-tex-tree
                    "no-param-tex-command has contents: ~s"
                    tree))]
          [else
           (display-tex-option/#f option)
           (display-tex-params/#f params)
           (printf "{")
           (display-tex-tree content)
           (printf "}")])
        (when (is-pretty-print-newline-tex-command? tag)
          (newline))]
       [else
        (printf "\\begin{~a}" tag)
        (display-tex-option/#f option)
        (display-tex-params/#f params)
        (match content
          [(cons (or (? string? s)
                     (cons (? string? s) _))
                 _)
           #:when (string-prefix? s "\n")
           (void)]
          [_
           (newline)])
        (display-tex-tree content)
        (printf "\\end{~a}\n" tag)])]
    [else (error 'display-tex-tree "cannot display ~s" tree)]))

(define (display-tex-option/#f option)
  (when option
    (printf "[~a]" option)))

(define (display-tex-params/#f params)
  (when params
    (for ([param (in-list params)])
      (printf "{~a}" param))))