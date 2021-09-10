#lang pollen/markup

◊(require (except-in "preamble.rkt" Gp)
          racket/list
          racket/contract
          racket/match

          txexpr
          (only-in pollen/tag default-tag-function))

◊(define Gp ◊text{Logs})

◊; Sometimes need \usebeamercolor{normal text} before \textcolor{fg}{...}
◊; metropolis normal text bg color is black!2
◊(define bg-color "bg")

◊(define (tikzmark/frame name)
  ◊string-append{
    \tikzmark{◊|name|◊get-frame-counter-name[]}
  })

◊(define (tikzpic name)
  (format "pic cs:~a~a" name (get-frame-counter-name)))

◊(define (add-box #:box [box fbox]
                  #:fboxsep [fboxsep "0pt"]
                  . content)
  ◊@{
    {◊setlength[#:param "\\fboxsep"]{◊fboxsep}◊box{
      ◊vphantom{()}◊(apply @ content)
    }}
  })

◊(define (textcolor-blue . content)
  (apply textcolor content #:param "blue"))

◊(define (add-box$ #:box [box fbox]
                   #:fboxsep [fboxsep "0pt"]
                   . content)
  (add-box (apply $ content)
           #:box box
           #:fboxsep fboxsep))

◊(define (colorbox-bg-color . content)
  (apply colorbox content #:param highlight-bg-color))

◊(define (highlight-box #:fboxsep [fboxsep "0pt"]
                        . content)
  (apply add-box
         content
         #:fboxsep fboxsep
         #:box colorbox-bg-color))

◊(define (highlight-box$ #:fboxsep [fboxsep "0pt"]
                        . content)
  (apply add-box$
         content
         #:fboxsep fboxsep
         #:box colorbox-bg-color))

◊(define (evolve-function-log-box . content)
  (textcolor-blue
   (apply add-box content #:fboxsep "1pt")))

◊(define highlight-bg-color "Lavender")

◊(define string-tree/c
   (flat-rec-contract string-tree/c
                      string?
                      (listof string-tree/c)))

◊(define (@strtree . content)
  (apply string-append (flatten content)))

◊(define/contract (strcolor color . content)
  (->* (string?) () #:rest string-tree/c string-tree/c)
  (list "\\textcolor{" color "}{" content "}"))

◊(define (con str) (strcolor "blue" str))
◊(define (sym str) (strcolor "red" str))
◊(define (codehl #:fboxsep [fboxsep #f]
                 #:color [color highlight-bg-color]
                 . content)
  (list "{"
        (if fboxsep
            (format "\\setlength{\\fboxsep}{~a}" fboxsep)
            '())
        "\\colorbox{" color "}{" content "}"
        "}"))

◊(define verbatim-left-margin "8mm")

◊(define/contract (example.prog1 #:comment [comment (hash)]
                                 #:process [process (hash)])
  (->* ()
       (#:comment (hash/c symbol? string-tree/c)
        #:process (hash/c symbol? (->* () #:rest string-tree/c string-tree/c)))
       any)
  (define (mark key)
    (hash-ref comment key (λ () "")))
  (define (prep key . content)
    (apply (hash-ref process key (λ () list)) content))
  ◊@strtree{
    (define (program.1 x y)     ◊(mark 'args)
     ◊(mark 'if1) (if ◊prep['cond1]{(< x 0)}               ◊(mark 'cond1)
     ◊(mark 'th1)    ◊(mark 'if2) (if ◊prep['cond2]{(= (* 4 (+ 1 y))}  ◊(mark 'cond2)
              ◊prep['cond3]{   (+ 7 (+ 1 (* 6 y))))}
         ◊(mark 'th2)    ◊(mark 'bug) (error "bug")     ◊(mark 'error)
         ◊(mark 'el2)    ◊(mark 'dots2) ...)              ◊(mark 'else2)
     ◊(mark 'el1)    ◊(mark 'dots1) ...))

  })

◊(define/contract (example.prog2 #:comment [comment (hash)]
                                 #:process [process (hash)])
  (->* ()
       (#:comment (hash/c symbol? string-tree/c)
        #:process (hash/c symbol? (->* () #:rest string-tree/c string-tree/c)))
       any)
  (define (mark key)
    (hash-ref comment key (λ () "")))
  (define (prep key . content)
    (apply (hash-ref process key (λ () list)) content))
  ◊@strtree{
     (define (program.2 f)               ◊(mark 'args)
       (if (= (* 4 ◊prep['app1]{(f (λ (y) y))})        ◊(mark 'app1)
              (+ 7 ◊prep['app2]{(f (λ (y) (* 6 y)))})) ◊(mark 'app2)
           ◊prep['error]{(error "4*f(\_)=7+f(6*\_)")}     ◊(mark 'error)
           ...))                         ◊(mark 'dots)
   })

◊(define/contract (example.prog1.concolic-test #:comment [comment (hash)])
  (->* () (#:comment (hash/c symbol? string-tree/c)) any)
  (define (mark key)
    (hash-ref comment key (λ () "")))
  ◊@strtree{
    (define-concolic-test T
     #:inputs [X integer][Y integer]◊(mark 'inputs)
     #:prop   (prop-not-exn         ◊(mark 'prop)
               (λ () (program.1 X Y))))

    (concolic-test T)

  })

◊title{Sound and Complete Concolic Testing for Higher-order Functions}
◊author{Shu-Hung You, Robert Bruce Findler and Christos Dimoulas}
◊date{March 31, ESOP 2021}
◊institute{PLT @ Northwestern University, USA}

◊frame{
◊titlepage[]
}

◊frame[#:option "fragile"]{
◊frametitle{Sound and Complete Concolic Testing for Higher-order Functions}

◊textbf{Concolic testing} ◊${=} testing ◊${+} symbolic execution

◊textbf{Challenge}: generate arbitrary higher-order functions

◊Verbatim[#:numbers "left"
          #:xleftmargin verbatim-left-margin
          #:commandchars verbatim-command-chars
          #:codes verbatim-math-codes
          #:verbatim "true"]{◊@strtree{
  (define (program.2 f)
    (if      ◊codehl[#:fboxsep "0pt"]{(f (λ (y) y))} $\dots$
        $\dots$ ◊codehl[#:fboxsep "0pt"]{(f (λ (y) (* 6 y)))}
                       $\dots$

}}

}

◊(define faded-color "LightGray")

◊frame[#:option "fragile"]{
◊frametitle{Sound and Complete Concolic Testing for Higher-order Functions}

◊textcolor[#:param faded-color]{
  ◊textbf{Concolic testing} ◊${=} testing ◊${+} symbolic execution
}

◊textcolor[#:param faded-color]{
  ◊textbf{Challenge}: generate arbitrary higher-order functions
}

◊Verbatim[#:numbers "left"
          #:xleftmargin verbatim-left-margin
          #:commandchars verbatim-command-chars
          #:codes verbatim-math-codes
          #:verbatim "true"]{◊@strtree{
  \textcolor{◊faded-color}{(define (program.2 f)}
  \textcolor{◊faded-color}{  (if      (f (λ (y) y)) $\dots$}
  \textcolor{◊faded-color}{      $\dots$ (f (λ (y) (* 6 y)))}
  \textcolor{◊faded-color}{                     $\dots$}

}}

◊textbf{Contribution}:
a ◊textbf{(relatively) complete} and ◊textbf{sound} concolic testing model for
    higher-order functions

* {◊small[] In a CBV functional language without mutable state}

}

◊(define first-order-concolic-execution-text.0
  ◊@{
    The concolic tester runs the program using ◊@strtree{◊con{concrete inputs}}.
  })

◊(define first-order-concolic-execution-text
  ◊@{
    - ◊first-order-concolic-execution-text.0
    - The results of `<`, `*`, etc., are
        annotated with ◊@strtree{◊sym{symbolic terms}}.
  })

◊; "Green" is from svgnames while 'green' is one of the base colors
◊(define negate-color "LimeGreen")
◊(define negate-color-dark "ForestGreen")
◊(define negate-box-style
  ◊string-append{\setlength{\fboxrule}{1.5pt}\setlength{\fboxsep}{1pt}})
◊(define (add-negate-box #:color [color negate-color] . content)
  ◊list{
    \textcolor{◊color}{◊|negate-box-style|\boxed{\textcolor{black}{◊content}}}
  })

◊(define first-order-concolic-execution-empty-highlight
  (hash
     'cond2
     (λ cond2
       ◊list{
         ◊(codehl cond2 #:fboxsep "2pt" #:color bg-color)
       })
     'cond3
     (λ cond3
       (codehl cond3 #:fboxsep "2pt" #:color bg-color))
     ))
◊(define (first-order-concolic-execution-empty-frame
          #:process [process first-order-concolic-execution-empty-highlight]
          #:comment [comment (hash)])

◊frame[#:option "fragile"]{
◊frametitle{First-order Concolic Testing}

◊Verbatim[#:xleftmargin verbatim-left-margin
          #:commandchars verbatim-command-chars
          #:codes verbatim-math-codes
          #:baselinestretch example.prog1.inputs.line-height
          #:verbatim "true"]{
  \vphantom{x $=$ $◊@strtree{◊con{-5}\vphantom{,◊sym{X}}}$}
  \vphantom{y $=$ $◊@strtree{◊add-negate-box[#:color bg-color]{◊con{0}}}$}

}
◊Verbatim[#:numbers "left"
          #:xleftmargin verbatim-left-margin
          #:commandchars verbatim-command-chars
          #:codes verbatim-math-codes
          #:verbatim "true"]{
  ◊(example.prog1
    #:process
    process
    #:comment
    comment
    )
}

◊vphantom{hidden paragraph}

◊vphantom{hidden paragraph}

◊vphantom{hidden paragraph}
◊raw-latex["\n"]

})

◊(first-order-concolic-execution-empty-frame)

◊(first-order-concolic-execution-empty-frame
  #:comment
  (hash
   'args
   ◊list{\textnormal{\textcolor{blue}{$\Longleftarrow \text{inputs}: x, y$}}}
   ))

◊(first-order-concolic-execution-empty-frame
  #:comment
  (hash
   'args
   ◊list{\textnormal{\textcolor{blue}{$\Longleftarrow \text{inputs}: x, y$}}}
   'error
   ◊list{\textnormal{\textcolor{blue}{$\Longleftarrow \text{crashes}$}}}
   ))

◊(define cfvc "[yshift=0.6ex]")
◊(define (example.prog1.pins)
  (hash
   'if1 (tikzmark/frame "if1")
   'th1 (tikzmark/frame "th1")
   'if2 (tikzmark/frame "if2")
   'th2 (tikzmark/frame "th2")
   'bug (tikzmark/frame "bug")
   'el2 (tikzmark/frame "el2")
   'dots2 (tikzmark/frame "dots2")
   ))
◊(define (example.prog1.control-flow)
  ◊tikzpicture[#:verbatim "true" #:option "remember picture,overlay"]{
    \draw[->,blue,thick,line width=1.5pt]
      (◊cfvc ◊tikzpic{if1}) --
      (◊cfvc ◊tikzpic{th1}) --
      (◊cfvc ◊tikzpic{if2});
    \draw[->,blue,thick,line width=1.5pt]
      (◊cfvc ◊tikzpic{if2}) --
      (◊cfvc ◊tikzpic{el2}) --
      (◊cfvc ◊tikzpic{dots2});

  })
◊(define (example.prog1.negated-control-flow)
  ◊tikzpicture[#:verbatim "true" #:option "remember picture,overlay"]{
    \draw[->,blue,thick,line width=1.5pt]
      (◊cfvc ◊tikzpic{if1}) --
      (◊cfvc ◊tikzpic{th1}) --
      (◊cfvc ◊tikzpic{if2});
    \draw[->,◊negate-color,thick,line width=1.5pt]
      (◊cfvc ◊tikzpic{if2}) --
      (◊cfvc ◊tikzpic{th2}) --
      (◊cfvc ◊tikzpic{bug});
    \draw[dotted,blue,thick,line width=1.5pt]
      (◊cfvc ◊tikzpic{th2}) --
      (◊cfvc ◊tikzpic{el2}) --
      (◊cfvc ◊tikzpic{dots2});

  })

◊(define example.prog1.inputs.line-height "0.85")

◊frame[#:option "fragile"]{
◊frametitle{First-order Concolic Testing}

◊Verbatim[#:xleftmargin verbatim-left-margin
          #:commandchars verbatim-command-chars
          #:codes verbatim-math-codes
          #:baselinestretch example.prog1.inputs.line-height
          #:verbatim "true"]{
  x $=$ $◊@strtree{◊con{-5}\vphantom{,◊sym{X}}}$
  y $=$ $◊@strtree{◊add-negate-box[#:color bg-color]{◊con{0}}}$

}
◊Verbatim[#:numbers "left"
          #:xleftmargin verbatim-left-margin
          #:commandchars verbatim-command-chars
          #:codes verbatim-math-codes
          #:verbatim "true"]{
  ◊(example.prog1
    #:process
    (hash
     'cond1
     (λ cond1
       ◊list{
         ◊(codehl cond1 #:fboxsep "0pt")\textnormal{ ◊con{\texttt{\#true}}}
       })
     'cond2
     (λ cond2
       ◊list{
         ◊(codehl (list cond2 "       ") #:fboxsep "2pt")\textnormal{ ◊con{\texttt{\#false}}}
       })
     'cond3
     (λ cond3
       (codehl cond3 #:fboxsep "2pt"))
     )
    #:comment
    (example.prog1.pins))
}

- ◊first-order-concolic-execution-text.0

◊vphantom{hidden paragraph}

◊(example.prog1.control-flow)

}

◊(define (first-order-concolic-execution-symbolic-frame
          #:process [process (hash)])

◊frame[#:option "fragile"]{
◊frametitle{
  ◊textcolor[#:param "lime"]{Concolic Evaluation}:
  Symbolic Execution with Modified Runtime
}

◊Verbatim[#:xleftmargin verbatim-left-margin
          #:commandchars verbatim-command-chars
          #:codes verbatim-math-codes
          #:baselinestretch example.prog1.inputs.line-height
          #:verbatim "true"]{
  x $=$ $◊@strtree{◊con{-5},◊sym{X}}$
  y $=$ $◊@strtree{◊add-negate-box[#:color bg-color]{◊con{0}},◊sym{Y}}$

}
◊Verbatim[#:numbers "left"
          #:xleftmargin verbatim-left-margin
          #:commandchars verbatim-command-chars
          #:codes verbatim-math-codes
          #:verbatim "true"]{
  ◊(example.prog1
    #:process
    process
    #:comment
    (example.prog1.pins))
}

◊first-order-concolic-execution-text

◊(example.prog1.control-flow)

})

◊(first-order-concolic-execution-symbolic-frame
  #:process
  (hash
    'cond1
    (λ cond1
      ◊list{
        ◊(codehl cond1 #:fboxsep "0pt")\textnormal{ ◊con{\texttt{\#true}},◊sym{$(X<0)$}}
      })
    'cond2
    (λ cond2
      ◊(list
        ◊(codehl (list cond2 "       ") #:fboxsep "2pt")
        ◊list{\textnormal{ ◊con{\texttt{\#false}},◊sym{$(4+4Y=8+6Y)$}}}
        ))
    'cond3
    (λ cond3
      (codehl cond3 #:fboxsep "2pt"))
    )
  )

◊(first-order-concolic-execution-symbolic-frame
  #:process
  (hash
    'cond1
    (λ cond1
      ◊list{
        ◊(codehl cond1 #:fboxsep "0pt" #:color bg-color)\textnormal{ ◊con{\texttt{\#true}},◊sym{$(X<0)$}}
      })
    'cond2
    (λ cond2
      ◊(list
        ◊(codehl (list "\\textcolor{blue}{" cond2 "}       ") #:fboxsep "2pt")
        ◊list{\textnormal{ ◊con{\texttt{\#false}},◊sym{$(4+4Y=8+6Y)$}}}
        ))
    'cond3
    (λ cond3
      (codehl (list "\\textcolor{blue}{" cond3 "}") #:fboxsep "2pt"))
    )
  )

◊(define example.prog1.crop-program
  (hash
   'cond3
   (λ cond3
     (match cond3
       [(list (? string? text))
        (list (substring text 0 (- (string-length text) 9))
              "...")]
        [_ cond3]))))
◊(define (example.prog1.add-logs #:process [process (hash)])
  (define (prep key . content)
    (apply (hash-ref process key (λ () list))
           content))
  (hash-set*
   (example.prog1.pins)
   'args
   ◊list{
     \textnormal{ \textbf{Logs:}}
   }
   'cond1
   ◊list{
     \textnormal{  if: ◊sym{$X<0$} is \texttt{◊con{\#true}}}
   }
   'cond2
   ◊list{
     \textnormal{  if: ◊sym{$(4+4Y=8+6Y)$} is ◊prep['log]{\texttt{◊con{\#false}}}}
   }
   'error
   ◊list{
     \textnormal{ \textbf{Formula:}}
   }
   'else2
   ◊list{
     \textnormal{     ◊prep['formula]{$(X<0)\land◊prep['negated]{\neg(4+4Y=8+6Y)}$}}
   }
   ))

◊(define (tracking-control-flow-frame #:process [process (hash)])

◊frame[#:option "fragile"]{
◊frametitle{Tracking Control Flow with Logs (Path Constraints)}

◊Verbatim[#:xleftmargin verbatim-left-margin
          #:commandchars verbatim-command-chars
          #:codes verbatim-math-codes
          #:baselinestretch example.prog1.inputs.line-height
          #:verbatim "true"]{
  x $=$ $◊@strtree{◊con{-5},◊sym{X}}$
  y $=$ $◊@strtree{◊add-negate-box[#:color bg-color]{◊con{0}},◊sym{Y}}$

}
◊Verbatim[#:numbers "left"
          #:xleftmargin verbatim-left-margin
          #:commandchars verbatim-command-chars
          #:codes verbatim-math-codes
          #:verbatim "true"]{
  ◊(example.prog1
    #:process
    example.prog1.crop-program
    #:comment
    (example.prog1.add-logs #:process process))
}

- The concolic tester logs the branching result.
- The logs are turned into a logic formula.

◊(example.prog1.control-flow)

})

◊(tracking-control-flow-frame)
◊(tracking-control-flow-frame
  #:process
  (hash
   'formula
   (λ content
     (codehl content #:fboxsep "0pt"))
   ))
◊(tracking-control-flow-frame)

◊frame[#:option "fragile"]{
◊frametitle{Evolution of the Inputs}

◊Verbatim[#:xleftmargin verbatim-left-margin
          #:commandchars verbatim-command-chars
          #:codes verbatim-math-codes
          #:baselinestretch example.prog1.inputs.line-height
          #:verbatim "true"]{
  x $=$ $◊@strtree{◊con{-5},◊sym{X}}$
  y $=$ $◊@strtree{◊add-negate-box{◊con{-2}},◊sym{Y}}$

}
◊Verbatim[#:numbers "left"
          #:xleftmargin verbatim-left-margin
          #:commandchars verbatim-command-chars
          #:codes verbatim-math-codes
          #:verbatim "true"]{
  ◊(example.prog1
    #:process
    example.prog1.crop-program
    #:comment
    (example.prog1.add-logs
     #:process
     (hash
      'log
      (λ content ◊list{
        ◊add-negate-box{\texttt{◊con{\#true}}}
      })
      'negated
      (λ content ◊list{
        ◊add-negate-box{◊strcolor[negate-color-dark]{\pmb{\neg}}\neg(4+4Y=8+6Y)}
      })
      )))
}

- By ◊textcolor[#:param negate-color-dark]{negating the formula} and consulting an SMT solver,
    the concolic tester constructs the new inputs and
    ◊textcolor[#:param negate-color-dark]{predicts the control flow
    in terms of logs}.

◊(example.prog1.negated-control-flow)

}

◊(define loop-simple-y-1 "1.3cm")
◊(define loop-simple-y-2 "0cm")
◊(define loop-simple-y-3 "-1.5cm")
◊(define loop-simple-x-1 "0cm")
◊(define loop-simple-x-2 "3.5cm")
◊(define loop-simple-x-3 "8cm")
◊(define (concolic-loop-simple  #:comment [comment (hash)]
                                #:process [process (hash)])
  (define (mark key)
    (hash-ref comment key (λ () (@))))
  (define (prep key . content)
    (apply (hash-ref process key (λ () @))
           content))
  ◊tikzpicture[#:verbatim "true"]{
    \node(user) at (◊loop-simple-x-2, ◊loop-simple-y-1) {
      ◊prep['user]{The User Program}
    };
    \node[rectangle, draw](eval) at (◊loop-simple-x-2, ◊loop-simple-y-2) {
      ◊minipage[#:option "t" #:param "3.7cm"]{
        ◊prep['eval]{Concrete and ◊\\[] Symbolic Execution}
      }
    };
    \node[rectangle, draw](evolve) at (◊loop-simple-x-2, ◊loop-simple-y-3) {
      ◊prep['evolve]{
        Evolution, ◊${◊mathit{evolve}}
      }
    };
    \node(ce) at (◊loop-simple-x-3,◊loop-simple-y-2) {
      ◊prep['ce]{Counter-example}
    };
    \draw[->,thick◊prep['user--eval]] (user) -- (eval);
    \draw[->,thick◊prep['eval--ce]] (eval) -- (ce);
    \draw[->,thick◊prep['eval--evolve]]
      (eval) --
      node[midway,right] {
        ◊prep['log]{Logs}
      }
      (evolve);
    \draw[->,thick◊prep['evolve--input]]
      (evolve) --
      (◊loop-simple-x-1,◊loop-simple-y-3) --
      node[pos=0.15,left] {
        ◊minipage[#:option "t" #:param "2.8cm"]{
          ◊prep['newinput]{New Inputs \\& Predicted Logs}
      }}
      (◊loop-simple-x-1,◊loop-simple-y-2);
    \draw[->,thick◊prep['input--eval]]
      (◊loop-simple-x-1,◊loop-simple-y-2) --
      node[pos=-0.1,above◊prep['input.node]] {
        ◊prep['input]{Inputs}
      }
      (eval);
  })

◊(define (completeness-frame #:filter [filter-list "0,1,2"]
                             #:faded [faded-list #f]
                             #:process [process (hash)])
◊frame[#:option "c"]{
◊frametitle{Completeness, Soundness and the Concolic Property}

◊vspace{-0.3em}
◊center{◊(concolic-loop-simple #:process process)}

◊selective[
  #:filter filter-list
  #:faded faded-list
  #:fadedcolor faded-color
]{
- ◊textbf{Completeness}: the model can find all crashes
    in concolic evaluation.

    {◊footnotesize[] Subject to the power of SMT solvers
     and the size of the search space.}

- ◊textbf{Soundness}: any crashing in concolic evaluation is a real one.
- ◊textbf{The Concolic Property}: the
    evolution of the inputs
    predicts the control flow of the user program.
}

})

◊(define concolic-loop-simple-faded
  (hash
   'user--eval
   (λ () ◊@{,draw=◊faded-color})
   'input--eval
   (λ () ◊@{,draw=◊faded-color})
   'eval--evolve
   (λ () ◊@{,draw=◊faded-color})
   'eval--ce
   (λ () ◊@{,draw=◊faded-color})
   'evolve--input
   (λ () ◊@{,draw=◊faded-color})
   'user
   (λ content
     ◊textcolor[#:param faded-color]{◊(apply @ content)})
   'input
   (λ content
     ◊textcolor[#:param faded-color]{◊(apply @ content)})
   'eval
   (λ content
     ◊textcolor[#:param faded-color]{◊(apply @ content)})
   'evolve
   (λ content
     ◊textcolor[#:param faded-color]{◊(apply @ content)})
   'newinput
   (λ content
     ◊textcolor[#:param faded-color]{◊(apply @ content)})
   'log
   (λ content
     ◊textcolor[#:param faded-color]{◊(apply @ content)})
   'ce
   (λ content
     ◊textcolor[#:param faded-color]{◊(apply @ content)})
   'concrete-error
   (λ content
     ◊textcolor[#:param faded-color]{◊(apply @ content)})
   'canonical-error
   (λ content
     ◊textcolor[#:param faded-color]{◊(apply @ content)})
   ))

◊(define concolic-loop-simple-staging
  (list
    (hash)
    (hash-set*
     concolic-loop-simple-faded
     'user--eval
     @
     'input--eval
     @
     'input
     @
     'user
     @
     'eval
     (λ content
       ◊textcolor[#:param "red"]{◊(apply @ content)})
     )
    (hash-set*
     concolic-loop-simple-faded
     'eval--evolve
     @
     'eval
     (λ content
       ◊textcolor[#:param "gray"]{◊(apply @ content)})
     'evolve
     (λ content
       ◊textcolor[#:param negate-color-dark]{
         ◊textbf{◊(apply @ content)}
       })
     'log
     (λ content
       ◊textcolor[#:param "black"]{◊(apply @ content)})
     )
    (hash-set*
     concolic-loop-simple-faded
     'eval--evolve
     @
     'evolve--input
     @
     'evolve
     (λ content
       ◊textcolor[#:param "black"]{◊(apply @ content)})
     'newinput
     (λ content
       ◊textcolor[#:param negate-color-dark]{
         ◊textbf{◊(apply @ content)}
       })
     'log
     (λ content
       ◊textcolor[#:param "gray"]{◊(apply @ content)})
     )
    (hash-set*
     concolic-loop-simple-faded
     'input--eval
     @
     'eval--ce
     @
     'input
     (λ content
       ◊textcolor[#:param "gray"]{◊(apply @ content)})
     'eval
     (λ content
       ◊textcolor[#:param "black"]{◊(apply @ content)})
     'ce
     (λ content
       ◊textcolor[#:param "blue"]{◊(apply @ content)})
     )
    (hash)
   ))

◊(apply
  @
  (for/list ([simple-staging (in-list concolic-loop-simple-staging)])
    (completeness-frame
     #:process simple-staging
     #:faded "0,1,2")))

◊(completeness-frame
  #:faded "1,2"
  #:process
  (hash
   'user--eval
   (hash-ref concolic-loop-simple-faded 'user--eval)
   'eval--ce
   (hash-ref concolic-loop-simple-faded 'eval--ce)
   'user
   (hash-ref concolic-loop-simple-faded 'user)
   'ce
   (hash-ref concolic-loop-simple-faded 'ce)
   )
  )

◊(completeness-frame
  #:faded "0,2"
  #:process
  (hash-set*
   concolic-loop-simple-faded
   'input--eval
   @
   'eval--ce
   @
   'input
   @
   'eval
   @
   'ce
   @
   )
  )

◊(completeness-frame
  #:faded "0,1"
  #:process
  (hash-set*
   concolic-loop-simple-faded
   'eval--evolve
   @
   'evolve--input
   @
   'log
   @
   'evolve
   @
   'newinput
   @
   )
  )

◊(define ntCF ◊mathit{CF})
◊(define ntcase ◊mathit{case})
◊(define lerror ◊textsf{◊small[] error})
◊(define lproc? ◊textsf{◊small[]procedure?})
◊(define lequ? ◊textsf{◊small[]equal?})
◊(define lcond ◊textbf{◊small[]cond})
◊(define llet ◊textbf{◊small[]let})
◊(define llistof ◊textbf{◊small[]listof})
◊(define ntec ◊string-append{e^\circ})
◊(define ntvc ◊string-append{v^\circ})
◊(define grammar-sep "::=")
◊(define (canonical-function-grammar #:process [process (hash)])
  (define (prep key . content)
    (apply (hash-ref process key (λ () @))
           content))
  ◊${
    ◊array[#:param "rcl"]{
      ◊prep['ntCF]{◊ntCF} &
        ◊prep['ntCF]{◊grammar-sep} &
        ◊prep['ntCF]{◊Gl ◊prep['bind1]{z}. ◊prep['inspect1]{◊ntcase_z}}
        ◊\\[]
      ◊prep['ntcase]{◊ntcase_z} & ◊prep['ntcase]{◊grammar-sep} &
        ◊prep['ntcase]{
        (◊lcond ◊SP
          ◊prep['clause]{◊prep['ntcase-clause1]{
            [(◊lproc? ◊SP ◊prep['ntcase-var]{z})
            ◊SP ◊prep['action1]{◊ntec}]
          }} ◊SP
          ◊prep['clause]{◊prep['ntcase-clause2]{
            [(◊lequ? ◊SP ◊prep['ntcase-var]{z} ◊SP ◊prep['symvar]{n})
            ◊SP ◊prep['action1]{◊ntec}]
          }}
          ◊dots[]
          ◊prep['clause]{◊prep['ntcase-clause3]{
            [(◊lequ? ◊SP ◊prep['ntcase-var]{z} ◊SP ◊prep['symvar]{n})
           ◊SP ◊prep['action1]{◊ntec}]
          }})
        }
          ◊\\[]
      ◊prep['ntec]{◊ntec} & ◊prep['ntec]{◊grammar-sep} &
        ◊prep['ntec]{
        ◊prep['action2]{◊prep['action2.1]{◊ntvc}} ◊SP | ◊SP
        ◊prep['action2]{◊prep['action2.2]{(◊llet ◊SP ([◊prep['bind2]{z} ◊SP (x ◊SP ◊ntvc)]) ◊SP
        ◊prep['inspect2]{◊ntcase_z})}}
        }
        ◊\\[]
      ◊prep['ntvc]{◊ntvc} & ◊prep['ntvc]{◊grammar-sep} &
        ◊prep['ntvc]{◊ntCF ◊SP | ◊SP n}
    }
  })

◊(define (canonical-function-text
          #:faded [faded-list ""]
          #:process [process (hash)])
  (define (prep key . content)
    (apply (hash-ref process key (λ () @))
           content))
  ◊selective[#:faded faded-list]{
    1. ◊prep['inspect]{◊${◊Gl}- and ◊${◊llet}-bindings are
        immediately inspected by ◊${◊ntcase} expressions.}
    2. ◊prep['action]{In ◊${◊ntcase} expressions, each ◊${e^\circ}
        can return or call a bound variable.}
  })

◊(define (completeness-exists-CF-text
          #:=> [=> ◊@{◊quad[] ◊Longrightarrow[] ◊quad[]}]
          #:process [process (hash)])
  (define (prep key . content)
    (apply (hash-ref process key (λ () @))
           content))
  (define CF ◊prep['CF]{◊ntCF})
  ◊@{◊${◊quad[] ◊array[#:param "@{}l@{}"]{
    ◊prep['concrete-error]{
      \underline{e}\\{X\mapsto ◊prep['lam]{(◊Gl x.e')}\\}\longrightarrow^* ◊lerror
    }
    ◊=>
    ◊prep['canonical-error]{
      ◊exists ◊CF. ◊SP
      \underline{e}\\{X\mapsto ◊CF \\}\longrightarrow^* ◊lerror
    }
  }}})

◊(define completeness-searching-text
  ◊@{
    ◊textbf{Searching}: construct ◊${◊ntCF_1, ◊ntCF_2, ◊dots[], ◊ntCF_n} using
    the ◊${◊mathit{evolve}} function.
  })

◊(define completeness-evolve-text
  ◊@{◊${◊array[#:param "@{}l@{}"]{
    ◊mathit{evolve} :
       ◊text{Inputs} ◊times[] ◊Gp
       ◊longrightarrow[]
       ◊llistof (◊text{Inputs} ◊times[] ◊Gp)
  }}})

◊(define (completeness-informally
          #:option [option/#f #f]
          #:filter [filter-list "0,1"]
          #:faded [faded-list ""]
          #:process [process concolic-loop-simple-faded])
  (define (prep key . content)
    (apply (hash-ref process key (λ () @))
           content))
  (keyword-apply
   frame
   (if option/#f (list '#:option) '())
   (if option/#f (list option/#f) '())
   ◊list{
◊frametitle{Completeness, informally}

◊center{◊(concolic-loop-simple #:process process)}

◊vspace{-0.5em}
◊textbf{Completeness}: the model can find all crashes.
◊vspace{-0.5em}
◊selective[
  #:filter filter-list
  #:faded faded-list
  #:fadedcolor faded-color
]{
1. ◊textbf{Canonical counterexamples} ◊cite{phil17,phil15}:

    ◊(completeness-exists-CF-text
      #:=> ◊@{◊sp ◊prep['canonical-error]{◊Longrightarrow[]} ◊\\[]}
      #:process process)

2. ◊completeness-searching-text

    ◊${◊quad[]} ◊completeness-evolve-text
}

}))

◊(completeness-informally
  #:option "c"
  #:filter "0,1"
  #:faded "0,1"
  #:process
  (hash-set*
   concolic-loop-simple-faded
   'concrete-error
   @
   'canonical-error
   @
   ))
◊(completeness-informally
  #:option "c"
  #:filter "0,1"
  #:faded "1"
  #:process
  (hash-set*
   concolic-loop-simple-faded
   'input
   textcolor-blue
   'input.node
   (λ () ",rectangle,draw=blue")
   ))
◊(completeness-informally
  #:option "c"
  #:filter "0,1"
  #:faded "1"
  #:process
  (hash-set*
   concolic-loop-simple-faded
   'input
   @
   'concrete-error
   @
   'lam
   (compose textcolor-blue highlight-box$)
   ))
◊(completeness-informally
  #:option "c"
  #:filter "0,1"
  #:faded "1"
  #:process
  (hash-set*
   concolic-loop-simple-faded
   'input
   @
   'concrete-error
   textcolor-blue
   ))
◊(completeness-informally
  #:option "c"
  #:filter "0,1"
  #:faded "1"
  #:process
  (hash-set*
   concolic-loop-simple-faded
   'input
   @
   'concrete-error
   (λ content
     ◊textcolor[#:param "gray"]{◊(apply @ content)})
   'canonical-error
   @
   'CF
   (compose textcolor-blue highlight-box$)
   ))
◊(completeness-informally
  #:option "c"
  #:filter "0,1"
  #:faded "1"
  #:process
  (hash-set*
   concolic-loop-simple-faded
   'input
   @
   'concrete-error
   (λ content
     ◊textcolor[#:param "gray"]{◊(apply @ content)})
   'canonical-error
   (λ content
     ◊textcolor[#:param "blue"]{◊(apply @ content)})
   ))
◊(completeness-informally
  #:option "c"
  #:filter "0,1"
  #:faded "0"
  #:process
  (hash-set*
   concolic-loop-simple-faded
   'eval--evolve
   @
  'evolve--input
   @
   'log
   @
   'evolve
   @
   'newinput
   @
   ))

◊(define (canonical-function-frame
          #:faded [faded-list ""]
          #:process [process (hash)])

◊frame[#:option "c"]{
◊frametitle{Canonical Functions (Incomplete Grammar)}

◊(canonical-function-text #:process process #:faded faded-list)

◊(canonical-function-grammar #:process process)

})

◊(define (textcolor-faded . content)
  ◊textcolor[#:param faded-color]{◊(apply @ content)})

◊(canonical-function-frame)

◊(canonical-function-frame
  #:faded "0,1"
  #:process
  (hash
   'ntcase
   textcolor-faded
   'ntec
   textcolor-faded
   'ntvc
   textcolor-faded
   ))

◊(canonical-function-frame
  #:faded "1"
  #:process
  (hash
   'ntCF
   textcolor-faded
   'ntec
   textcolor-faded
   'ntvc
   textcolor-faded
   ))

◊(canonical-function-frame
  #:faded "1"
  #:process
  (hash
   'action1
   highlight-box$
   'ntCF
   textcolor-faded
   'ntec
   textcolor-faded
   'ntvc
   textcolor-faded
   ))

◊(canonical-function-frame
  #:faded "0"
  #:process
  (hash
   'ntCF
   textcolor-faded
   'ntvc
   textcolor-faded
   ))

◊(canonical-function-frame
  #:faded "0"
  #:process
  (hash
   'action2.1
   highlight-box$
   'ntCF
   textcolor-faded
   ))

◊(canonical-function-frame
  #:faded "0"
  #:process
  (hash
   'action2.2
   highlight-box$
   'ntCF
   textcolor-faded
   'ntvc
   textcolor-faded
   ))

◊(canonical-function-frame
  #:faded "1"
  #:process
  (hash
   'bind2
   highlight-box$
   'inspect2
   highlight-box$
   'ntCF
   textcolor-faded
   'ntvc
   textcolor-faded
   ))

◊(define ind ◊@{◊SP ◊SP ◊SP ◊SP})
◊(define (case-log at-var val branch
                   #:process [process (hash)])
  (define (prep key . content)
    (apply (hash-ref process key (λ () @))
           content))
  ◊text{case: ◊prep['val]{◊val} is ◊prep['branch]{◊branch} at ◊prep['var]{◊at-var}})

◊(define (taste-of-canonical-function #:process [process (hash)])
  (define (prep key . content)
    (apply (hash-ref process key (λ () @))
           content))

◊frame[#:option "c"]{
◊frametitle{A Taste of Canonical Functions}

◊(canonical-function-grammar #:process process)

◊center{
  ◊renewcommand[#:param "\\arraystretch"]{0.9}
  ◊${
      ◊array[#:param "l" #:verbatim "true"]{
        ◊prep['bind1]{◊Gl h.} (◊lcond ◊\\[]
        ◊ind ◊ind [(◊lproc? ◊sp h) ◊\\[]
        ◊ind ◊ind ◊sp ◊prep['bind2]{(◊llet ◊SP ([z} ◊SP ◊prep['app]{(h ◊SP {-2})}]) ◊\\[]
        ◊ind ◊ind ◊ind
          (◊lcond ◊\\[]
        ◊ind ◊ind ◊ind ◊SP ◊SP ◊sp
          ◊prep['cl2.1]{[(◊lequ? ◊sp z ◊SP ◊SP {-2}) ◊SP ◊prep['action3.1]{◊SP ◊sp {-1}}]} ◊\\[]
        ◊ind ◊ind ◊ind ◊SP ◊SP ◊sp
          ◊prep['cl2.2]{[(◊lequ? ◊sp z ◊sp {-12}) ◊SP ◊prep['action3.2]{{-11}}]}))])
    }
  }
}

})

◊(taste-of-canonical-function
  #:process
  (hash
   ))

◊(taste-of-canonical-function
  #:process
  (hash
   'bind1
   highlight-box$
   'inspect1
   highlight-box$
   'bind2
   highlight-box$
   'inspect2
   highlight-box$
   'ntcase
   textcolor-faded
   'ntvc
   textcolor-faded
   ))

◊(taste-of-canonical-function
  #:process
  (hash
   'app
   highlight-box$
   'ntCF
   textcolor-faded
   'ntcase
   textcolor-faded
   'action2.2
   highlight-box$
   'ntvc
   textcolor-faded
   ))

◊(taste-of-canonical-function
  #:process
  (hash
   'inspect2
   highlight-box$
   'cl2.1
   highlight-box$
   'cl2.2
   highlight-box$
   'ntCF
   textcolor-faded
   'ntvc
   textcolor-faded
   ))

◊(taste-of-canonical-function
  #:process
  (hash
   'action3.1
   highlight-box$
   'action3.2
   highlight-box$
   'ntCF
   textcolor-faded
   'ntcase
   textcolor-faded
   'action2.1
   highlight-box$
   ))

◊(define (evolve-function-frame
          #:option [option "c"]
          #:process [process (hash)]
          #:filter [filter-list "0,1"]
          #:faded [faded-list ""])

◊frame[#:option option]{
◊frametitle{Evolution of Functional Inputs: Logs}

◊completeness-searching-text ◊\\[]
◊completeness-evolve-text

◊(canonical-function-grammar
  #:process process)

Observation: ◊${◊ntcase_z} expressions are
like ◊texttt{if} expressions.

◊selective[
  #:filter filter-list
  #:faded faded-list
  ]{
- ◊textbf{Logs}: if: ◊${X<0} is ◊texttt{\\#true}
- ◊textbf{Logs}: ◊(case-log ◊${z} ◊${v} ◊${n} #:process process)
}

})

◊(evolve-function-frame
  #:process
  (hash
   'ntCF
   textcolor-faded
   'ntcase
   textcolor-faded
   'ntec
   textcolor-faded
   'ntvc
   textcolor-faded
   )
  #:faded "0,1"
  )

◊(evolve-function-frame
  #:process
  (hash
   'ntCF
   textcolor-faded
   'ntec
   textcolor-faded
   'ntvc
   textcolor-faded
   )
  #:faded "0,1"
  )

◊(evolve-function-frame
  #:process
  (hash
   'ntCF
   textcolor-faded
   'ntec
   textcolor-faded
   'ntvc
   textcolor-faded
   )
  #:faded "1"
  )

◊(evolve-function-frame
  #:process
  (hash
   'ntCF
   textcolor-faded
   'ntec
   textcolor-faded
   'ntvc
   textcolor-faded
   )
  #:faded "0"
  )

◊(evolve-function-frame
  #:process
  (hash
   'ntCF
   textcolor-faded
   'ntec
   textcolor-faded
   'ntvc
   textcolor-faded
   'val
   evolve-function-log-box
   )
  #:faded "0"
  )

◊(evolve-function-frame
  #:process
  (hash
   'ntCF
   textcolor-faded
   'ntec
   textcolor-faded
   'ntvc
   textcolor-faded
   'ntcase-clause2
   highlight-box$
   'branch
   evolve-function-log-box
   )
  #:faded "0"
  )

◊(evolve-function-frame
  #:process
  (hash
   'ntCF
   textcolor-faded
   'ntec
   textcolor-faded
   'ntvc
   textcolor-faded
   'ntcase-var
   (compose textcolor-blue highlight-box$)
   'var
   evolve-function-log-box
   )
  #:faded "0"
  )

◊(define (evolve-log-frame #:process process)
  (define (prep key . content)
    (apply (hash-ref process key)
           content))

◊frame[#:option "fragile,c"]{
◊frametitle{Evolution of Functional Inputs: Logs}

◊Verbatim[#:numbers "left"
          #:xleftmargin verbatim-left-margin
          #:commandchars verbatim-command-chars
          #:codes verbatim-math-codes
          #:verbatim "true"]{◊@strtree{
  x $=$ $◊con{-5},◊sym{X}$
  f $=$ \textnormal{$\lambda{}z.(◊tikzmark/frame{fcasez}\textbf{\small{}cond} [\dots] [(\textsf{\small{}equal?} z {-2}) \dots] [\dots])$}

}}
◊Verbatim[#:numbers "left"
          #:xleftmargin verbatim-left-margin
          #:commandchars verbatim-command-chars
          #:codes verbatim-math-codes
          #:verbatim "true"]{◊@strtree{
  (define (program.3 x f)          \textnormal{\textbf{Logs:}}
    ... (if (< x 0)                \textnormal{ if: ◊sym{$X<0$} is ◊con{\texttt{\#true}}}
                 ...

              ◊prep['app]{(f (+ x 3))} ...  ◊prep['eval]
                                   ◊tikzmark/frame{logcasez}\textnormal{ case: ◊sym{$X+3$} is ${-2}$ at $z$}

             ... (if ...)          \textnormal{ if: $\dots$}
            ))
       ...
}}

◊prep['frame]
◊raw-latex["\n"]

})

◊(evolve-log-frame
  #:process
  (hash
   'app
   list
   'eval
   list
   'frame
   (λ content
     ◊vphantom{hidden paragraph})
   ))

◊(evolve-log-frame
  #:process
  (hash
   'app
   (λ content
     ◊list{
       ◊codehl[#:fboxsep "0pt"]{◊content}
     })
   'eval
   (λ content
     ◊list{
      \textnormal{\textcolor{blue}{$(\textbf{\small{}cond} [\dots(\textsf{\small{}equal?} z {-2})\dots])$}}
     })
   'frame
   (λ content
     ◊vphantom{hidden paragraph})
   ))

◊(evolve-log-frame
  #:process
  (hash
   'app
   list
   'eval
   (λ content
     ◊list{
      \textnormal{\textcolor{blue}{$(\textbf{\small{}cond} [\dots(\textsf{\small{}equal?} z {-2})\dots])$}}
     })
   'frame
   (λ content
    ◊@{
      ◊vphantom{hidden paragraph}
      ◊tikzpicture[#:verbatim "true" #:option "remember picture,overlay"]{
          \draw[->,blue,thick,line width=1.5pt]
            ([xshift=2ex,yshift=-0.2ex]◊tikzpic{fcasez}) --
            (◊cfvc ◊tikzpic{logcasez});

      }
    })
   ))

◊(define (evolve-pick-branch-frame
          #:process [process (hash)])
  (define (prep key . content)
    (apply (hash-ref process key (λ () @))
           content))

◊frame{
◊frametitle{Evolution of Functional Inputs (Case I)}

◊colorbox[#:param highlight-bg-color]{Pick a different branch} or create a new clause.

◊(canonical-function-grammar
  #:process
  process
  )

◊center{
  ◊renewcommand[#:param "\\arraystretch"]{0.9}
  ◊${
    ◊array[#:param "l"]{
      ◊textbf{Log:} ◊sp
        ◊prep['old-log]{◊(case-log ◊${z} ◊${v} ◊${n_3})}
      ◊\\[]
      ◊Gl ◊sp z. (◊lcond ◊\\[]
      ◊ind ◊ind [(◊lproc? ◊SP z) ◊SP ◊ntec_0] ◊\\[]
      ◊ind ◊ind [(◊lequ? ◊SP z ◊SP ◊@strtree{n_1})
                  ◊SP ◊ntec_1] ◊\\[]
      ◊ind ◊ind [(◊lequ? ◊SP z ◊SP ◊@strtree{n_2})
                  ◊SP ◊ntec_2] ◊\\[]
      ◊ind ◊ind ◊textcolor[#:param "blue"]{
                  ◊prep['old-clause]{[(◊lequ? ◊SP z ◊SP ◊@strtree{n_3})
                    ◊SP ◊ntec_3]}
                })
    }
    ◊SP ◊Longrightarrow[] ◊SP
    ◊array[#:param "l"]{
      ◊textbf{Log:} ◊sp
        ◊prep['new-log]{◊(case-log ◊${z} ◊${v} ◊${n_1})}
      ◊\\[]
      ◊Gl ◊sp z. (◊lcond ◊\\[]
      ◊ind ◊ind [(◊lproc? ◊SP z) ◊SP ◊ntec_0] ◊\\[]
      ◊ind ◊ind ◊prep['new-clause]{[(◊lequ? ◊SP z ◊SP ◊@strtree{n_1})
                  ◊SP ◊ntec_1]}
                ◊\\[]
      ◊ind ◊ind ◊prep['new-clause2]{[(◊lequ? ◊SP z ◊SP ◊@strtree{n_2})
                  ◊SP ◊ntec_2]}
                ◊\\[]
      ◊ind ◊ind [(◊lequ? ◊SP z ◊SP ◊@strtree{n_3})
                  ◊SP ◊ntec_3])
    }
  }
}

})

◊(evolve-pick-branch-frame
  #:process
  (hash
   'ntCF
   textcolor-faded
   'ntec
   textcolor-faded
   'ntvc
   textcolor-faded
   'new-clause
   textcolor-blue
   ))

◊(evolve-pick-branch-frame
  #:process
  (hash
   'ntCF
   textcolor-faded
   'ntec
   textcolor-faded
   'ntvc
   textcolor-faded
   'clause
   highlight-box$
   'new-clause
   textcolor-blue
   ))

◊(evolve-pick-branch-frame
  #:process
  (hash
   'ntCF
   textcolor-faded
   'ntec
   textcolor-faded
   'ntvc
   textcolor-faded
   'old-log
   highlight-box$
   'old-clause
   highlight-box$
   'new-clause
   textcolor-blue
   ))

◊(evolve-pick-branch-frame
  #:process
  (hash
   'ntCF
   textcolor-faded
   'ntec
   textcolor-faded
   'ntvc
   textcolor-faded
   'new-log
   (λ old-log
     ◊(case-log ◊${z}
                ◊${v}
                ◊highlight-box${n_1}))
   'new-clause
   (compose textcolor-blue highlight-box$)
   ))

◊(evolve-pick-branch-frame
  #:process
  (hash
   'ntCF
   textcolor-faded
   'ntec
   textcolor-faded
   'ntvc
   textcolor-faded
   'new-log
   (λ old-log
     ◊(case-log ◊${z}
                ◊${v}
                ◊highlight-box${n_2}))
   'new-clause2
   (compose textcolor-blue highlight-box$)
   ))

◊(define (evolve-new-clause-text
          #:process [process (hash)])
  (define (prep key . content)
    (apply (hash-ref process key (λ () @))
           content))

  ◊@{
    ◊center{
      ◊renewcommand[#:param "\\arraystretch"]{0.9}
      ◊${
        ◊array[#:param "l"]{
          ◊textbf{Log:} ◊sp
            ◊(case-log ◊${z} ◊${v^*} ◊${\\_})
          ◊\\[]
          ◊Gl ◊sp z. ◊\\[]
          ◊SP (◊lcond ◊\\[]
          ◊ind [(◊lproc? ◊SP z) ◊SP ◊ntec_0] ◊\\[]
          ◊ind [(◊lequ? ◊SP z ◊SP ◊@strtree{n_1})
                      ◊SP ◊ntec_1] ◊\\[]
          ◊ind [(◊lequ? ◊SP z ◊SP ◊@strtree{n_2})
                      ◊SP ◊ntec_2])
          ◊\\[]
          ◊hphantom{◊ind [◊ntec]}
        }
        ◊unsp ◊Longrightarrow[] ◊unsp ◊unsp ◊unsp ◊unsp
        ◊array[#:param "l"]{
          ◊textbf{Log:} ◊sp
            ◊prep['new-log]{◊(case-log ◊${z} ◊${v^*} ◊${\\_\\!\\_})}
          ◊\\[]
          ◊Gl ◊sp z. ◊\\[]
          ◊SP ◊sp (◊lcond ◊\\[]
          ◊ind [(◊lproc? ◊SP z) ◊SP ◊ntec_0] ◊\\[]
          ◊ind [(◊lequ? ◊SP z ◊SP ◊@strtree{n_1})
                      ◊SP ◊ntec_1] ◊\\[]
          ◊ind [(◊lequ? ◊SP z ◊SP ◊@strtree{n_2})
                      ◊SP ◊ntec_2] ◊\\[]
          ◊ind ◊prep['new-clause]{
                      [(◊lequ? ◊SP z ◊SP v^*)
                        ◊SP ◊prep['new-action1]{◊ntec_3}]
                    })
        }
        ◊unsp ◊unsp
        ◊array[#:param "l"]{
          ◊prep['new-text]{◊text{where} ◊sp ◊prep['new-action2]{◊ntec_3} ◊sp
          ◊text{is one of} ◊sp ◊prep['new-ntvc]{◊ntvc}}
          ◊sp ◊prep['new-text]{◊text{or}} ◊\\[]
          ◊prep['new-text]{◊prep['new-let]{(◊llet ◊sp ([z' ◊sp (x ◊sp ◊ntvc)]) ◊sp (◊lcond))}}
        }
      }
    }
  })

◊(define (evolve-new-clause-frame
          #:process [process (hash)])

◊frame{
◊frametitle{Evolution of Functional Inputs (Case II)}
Pick a different branch or
◊colorbox[#:param highlight-bg-color]{create a new clause}.

◊(canonical-function-grammar
  #:process
  process)

◊(evolve-new-clause-text
  #:process
  process)

})

◊(evolve-new-clause-frame
  #:process
  (hash
   'ntCF textcolor-faded
   'ntec textcolor-faded
   'ntvc textcolor-faded
   'new-clause textcolor-faded
   'new-text textcolor-faded
   'new-ntvc
   (λ content
     ◊textcolor[#:param bg-color]{
       ◊add-box$[#:fboxsep "1pt"]{◊(apply textcolor-faded content)}
     })
   'new-let
   (λ content
     ◊textcolor[#:param bg-color]{
       ◊add-box$[#:fboxsep "1pt"]{◊(apply textcolor-faded content)}
     })
   ))

◊(evolve-new-clause-frame
  #:process
  (hash
   'ntCF textcolor-faded
   'ntec textcolor-faded
   'ntvc textcolor-faded
   'clause
   highlight-box$
   'new-clause
   (compose textcolor-blue highlight-box$)
   'new-log
   (λ content
     ◊(case-log ◊${z}
                ◊${v^*}
                ◊highlight-box${v^*}))
   'new-text textcolor-faded
   'new-ntvc
   (λ content
     ◊textcolor[#:param bg-color]{
       ◊add-box$[#:fboxsep "1pt"]{◊(apply textcolor-faded content)}
     })
   'new-let
   (λ content
     ◊textcolor[#:param bg-color]{
       ◊add-box$[#:fboxsep "1pt"]{◊(apply textcolor-faded content)}
     })
   ))

◊(evolve-new-clause-frame
  #:process
  (hash
   'ntCF textcolor-faded
   'ntec textcolor-faded
   'ntvc textcolor-faded
   'action1
   highlight-box$
   'new-log
   (λ content
     ◊(case-log ◊${z} ◊${v^*} ◊${v^*}))
   'new-action1
   highlight-box$
   'new-text textcolor-faded
   'new-ntvc
   (λ content
     ◊textcolor[#:param bg-color]{
       ◊add-box$[#:fboxsep "1pt"]{◊(apply textcolor-faded content)}
     })
   'new-let
   (λ content
     ◊textcolor[#:param bg-color]{
       ◊add-box$[#:fboxsep "1pt"]{◊(apply textcolor-faded content)}
     })
   ))

◊(evolve-new-clause-frame
  #:process
  (hash
   'ntCF textcolor-faded
   'new-log
   (λ content
     ◊(case-log ◊${z} ◊${v^*} ◊${v^*}))
   'new-action1
   (λ content
     ◊textcolor[#:param "blue"]{◊(apply @ content)})
   'new-action2
   (λ content
     ◊textcolor[#:param "blue"]{◊(apply @ content)})
   'action2
   highlight-box$
   'new-ntvc
   (λ content
     ◊evolve-function-log-box{◊(apply $ content)})
   'new-let
   (λ content
     ◊textcolor[#:param bg-color]{
       ◊add-box$[#:fboxsep "1pt"]{
         ◊(apply textcolor content
                 #:param "black")
       }
     })
   ))

◊(evolve-new-clause-frame
  #:process
  (hash
   'ntCF textcolor-faded
   'ntvc textcolor-faded
   'new-log
   (λ content
     ◊(case-log ◊${z} ◊${v^*} ◊${v^*}))
   'new-action1
   (λ content
     ◊textcolor[#:param "blue"]{◊(apply @ content)})
   'new-action2
   (λ content
     ◊textcolor[#:param "blue"]{◊(apply @ content)})
   'action2
   highlight-box$
   'new-ntvc
   (λ content
     ◊textcolor[#:param bg-color]{
       ◊add-box$[#:fboxsep "1pt"]{
         ◊(apply textcolor content
                 #:param "black")
       }
     })
   'new-let
   (λ content
     ◊evolve-function-log-box{◊(apply $ content)})
   ))

◊frame[#:option "c"]{
◊frametitle{Evolution of Functional Inputs}

◊completeness-searching-text

◊noindent[]
◊completeness-evolve-text

- ◊textbf{Logs}: ◊(case-log ◊${z} ◊${v} ◊${n})
- Pick a different branch or create a new clause.

}

◊frame[#:option "c"]{
◊frametitle{Conclusion}

◊center{
  ◊minipage[#:param "\\textwidth"]{
    - We designed a model for concolic testing higher-order functions.
    - The model is sound and complete with respect to bug finding.
    - The input evolution process satisfies the concolic property.
    ◊;- The model allows for interoperability with
    ◊;    external libraries through ◊emph{concretization}.
  }
}

}

◊frame{
◊frametitle{References}

◊thebibliography[#:param "xxxxxxxx" #:verbatim "true"]{
  ◊bibitem[#:option "Nguy\\~{\\^{e}}n et al. 2015"]{phil15}
    Ph\'{u}c Nguy\~{\^{e}}n, Sam Tobin-Hochstadt, and David Van Horn
    ◊newblock[] Relatively complete counterexamples for higher-order programs.
    ◊newblock[] In ◊emph{PLDI'2015}.
  ◊bibitem[#:option "Nguy\\~{\\^{e}}n et al. 2017"]{phil17}
    Ph\'{u}c Nguy\~{\^{e}}n, Sam Tobin-Hochstadt, and David Van Horn
    ◊newblock[] Higher order symbolic execution for contract verification and refutation.
    ◊newblock[] In ◊emph{JFP'17}.
}

}

