#lang pollen/markup

◊(require "preamble.rkt")

◊(define (subst e v x)
   ◊@{◊e ◊unsp ◊bigparens[#:left "[" #:right "]"]{◊v / ◊x}})

◊(define arith-lang-defn-informal
   ◊@{算術語言中，一個程式即為一個由加法、乘法等二元運算和自然數組成的算式。
      此算式計算順序為由左向右。
      我們並規定算術語言的語法符合四則運算優先順序及左結合的慣例，
      同時允許自由的排版。})

◊(define (arith-lang-defn-ast #:with-e? [with-e? #f])
   (define a -2.8)
   (define b 0)
   (define c 3.2)
   (define e
     (if with-e? "$e$" ""))    ;; "
   (define subtree-yshift
     (if with-e? "-0.005cm" "-0.015cm"))
   (define node-yshift
     (if with-e? "1.0cm" "0.62cm"))
   (define tree-y
     (if with-e? "0.9cm" "0.7cm"))
   ◊center{◊tikzpicture[
     #:option
     ◊string-append{
       lsubtree/.style={
         isosceles triangle,
         isosceles triangle apex angle=30,
         shape border rotate=90,
         minimum width=0.5cm,
         xshift=0.1cm,yshift=◊subtree-yshift
       },
       rsubtree/.style={
         isosceles triangle,
         isosceles triangle apex angle=30,
         shape border rotate=90,
         minimum width=0.5cm,
         xshift=-0.1cm,yshift=◊subtree-yshift
       }
     }]{◊raw-latex{
     \node at (◊~a[(- a 0.8)]cm, 0) {(a)};
     \node[circle,draw] at (◊~a[a], 0) {$n$};

     \node at (◊~a[(- b 1.2)]cm, 0) {(b)};
     \node[circle,draw]
       at(◊~a[b]cm, ◊tree-y) { $\ttplus$ }
       child { node[yshift=◊node-yshift] {} {node[lsubtree,draw] {◊e}} }
       child { node[yshift=◊node-yshift] {} {node[rsubtree,draw] {◊e}} }
       ;

     \node at (◊~a[(- c 1.2)]cm, 0) {(c)};
     \node[circle,draw]
       at(◊~a[c]cm, ◊tree-y) { $\tttimes$ }
       child { node[yshift=◊node-yshift] {} {node[lsubtree,draw] {◊e}} }
       child { node[yshift=◊node-yshift] {} {node[rsubtree,draw] {◊e}} }
       ;
  }}})

◊(define (arith-lang-defn #:value? [value? #f] #:nat? [nat? #t])
   (define value-ast
     (cond
      [(not value?) ""]
      [(equal? value? 'alert) ;'
       ◊@{◊br
            ◊alert{◊Value} & ◊alert{◊ni[]} & ◊alert{v} & ◊alert{◊grammar-rule} &
            ◊alert{n}}]
      [else
       ◊@{◊br
            ◊Value & ◊ni[] & v & ◊grammar-rule &
            n}]))
   (define nat-ast
     (cond
       [(not nat?) ""]
       [else
        ◊@{◊br
             ◊bbN & ◊ni{} & n & ◊grammar-rule &
             0 ◊grammar-sep 1 ◊grammar-sep 2 ◊grammar-sep ◊dots[]}]))
   ◊$${
     ◊array[#:param "rcccl"]{
       ◊Term & ◊ni{} & e & ◊grammar-rule &
        n ◊grammar-sep
        e ◊ttplus e ◊grammar-sep
        e ◊tttimes e
       ◊value-ast
       ◊nat-ast
     }
   })

◊(define arith-lang-redexes
   ◊$${
     ◊array[#:param "ll"]{
       (n_1 ◊ttplus n_2) ◊mredex n &
         ◊text{for all}◊sp n_1, n_2, n ◊in[] ◊bbN ◊sp ◊text{ s.t. } ◊sp
         n=n_1+n_2
         ◊br
       (n_1 ◊tttimes n_2) ◊mredex n &
         ◊text{for all}◊sp n_1, n_2, n ◊in[] ◊bbN ◊sp ◊text{ s.t. } ◊sp
         n=n_1◊times[] n_2
     }
   }) ◊;{.}

◊(define (arith-let x e_1 e_2)
   ◊@{◊mathtt{let}◊sp ◊x ◊sp ◊tteq ◊sp ◊e_1 ◊sp
      ◊mathtt{in} ◊sp ◊e_2})

◊(define (aid x)
   ◊mathtt{◊x})

◊(define arith-var-lang-defn
   ◊$${◊aligned{
     & ◊array[#:param "rcccl"]{
       ◊Term & ◊ni{} & e & ◊grammar-rule &
        ◊dots{} ◊grammar-sep
        x ◊grammar-sep
        ◊arith-let[◊@{x} ◊@{e} ◊@{e}]
        ◊br
       ◊Var  & ◊ni[] & x, y, z, ◊dots[] & ◊grammar-rule &
        ◊aid{x} ◊grammar-sep
        ◊aid{y} ◊grammar-sep
        ◊aid{z} ◊grammar-sep ◊dots[]
     } ◊br
     & ◊text{◊${◊Var} is a countable set of variables. It is left abstract.}
   }})

◊(define tt ◊mathtt{true})
◊(define ff ◊mathtt{false})
◊(define (arith-if e_1 e_2 e_3)
   ◊@{◊mathtt{if}◊Sp ◊e_1 ◊Sp
      ◊mathtt{then}◊Sp ◊e_2 ◊Sp
      ◊mathtt{else}◊Sp ◊e_3})

◊(define arith-bool-lang-defn
   ◊$${
     ◊array[#:param "rccrl"]{
       ◊Term & ◊ni{} & e & ◊grammar-rule &
        ◊dots{} ◊grammar-sep
        b ◊grammar-sep
        e ◊tteq e  ◊grammar-sep
        ◊arith-if[◊@{e} ◊@{e} ◊@{e}]
        ◊br
       ◊Value & ◊ni[] & v & ◊grammar-rule &
        ◊dots[] ◊grammar-sep
        b
        ◊br 
       ◊bbB & ◊ni[] & b & ◊grammar-rule &
        ◊tt ◊grammar-sep
        ◊ff
     }
   })

◊(define (arith-type-defn #:show-Γ? [show-gamma? #f]
                          #:show-type? [show-type? #t])
   ◊$${
     ◊array[#:param "rccrl"]{
       ◊(cond
         [(not show-type?) ""]
         [else
          ◊@{
            Type & ◊ni{} & ◊Gt & ◊grammar-rule &
              ◊bbN ◊grammar-sep
              ◊bbB
          }])
       ◊(if (and show-type? show-gamma?) br "")
       ◊(cond
         [(not show-gamma?) ""]
         [else
          ◊@{
            ◊Ctxt & ◊ni{} & ◊GG & ◊grammar-rule &
            (x_1:◊Gt_1),◊dots[],(x_n:◊Gt_n)
          }])
     }
   })

◊author{}
◊date{2020 邏輯、語言與計算暑期研習營}
◊title{程式語言理論}
◊subtitle{導論：建立程式語言的模型}

◊raw-latex{
  {◊; deleted the background image (not relevant)
   ◊;\usebackgroundtemplate{\includegraphics[width=\paperwidth]{flolac-logo-image.png}}
   \begin{frame}\titlepage{}\end{frame}}
}

◊AtBeginSubsection[#:option ""]{
◊frame{◊tableofcontents[
  #:option
  "currentsection,currentsubsection,subsectionstyle=show/shaded/hide"
]}
}

# 為什麼研究程式語言？

◊subsection*{}

◊frame[#:option "fragile"]{
◊frametitle{程式語言建立抽象介面：高階語言}

程式語言建立抽象的界面，並藉由略去無關細節，讓
程式設計師直接描述並解決問題本身。在電腦系統中，
人們已經為不同層次的抽象化設計了對應的程式語言。

◊vspace{-1em}
◊minipage[#:option "t" #:param "\\textwidth"]{
  ◊minipage[#:param "0.5\\textwidth"]{
    ◊Verbatim[#:option "fontsize=\\scriptsize" #:verbatim "true"]{
      int maximum(int a, int b) {
        if (a >= b)
          return a;
        return b*b - 4;
      }

    }
  }
  ◊minipage[#:param "0.4\\textwidth"]{
    ◊Verbatim[#:option "fontsize=\\scriptsize" #:verbatim "true"]{
              .globl  maximum
      maximum:
              mov     eax, edi
              cmp     edi, esi
              jge     .L1
              imul    esi, esi
              lea     eax, [rsi-4]
      .L1:
              ret

    }
  }
}

◊Verbatim[#:option "fontsize=\\scriptsize" #:verbatim "true"]{
  define dso_local i32 @maximum(i32, i32) local_unnamed_addr #0 {
    %3 = icmp slt i32 %0, %1
    %4 = mul nsw i32 %1, %1
    %5 = add nsw i32 %4, -4
    %6 = select i1 %3, i32 %5, i32 %0
    ret i32 %6
  }

}
}

◊frame[#:option "fragile"]{
◊frametitle{程式語言建立抽象介面：各式語言特性}

程式語言也包含對不同應用抽象化設計的語言特性
與函式庫。

◊Verbatim[#:option "fontsize=\\scriptsize" #:verbatim "true"]{
  SELECT author FROM commit_list
    ORDER BY commit_time DESC
    WHERE ci_result='pass'
    LIMIT 10;

}

◊Verbatim[#:option "fontsize=\\scriptsize" #:verbatim "true"]{
def seq(Gs):
  for G in Gs:
    yield from G()
def replicate(G, n):
  yield from seq(G for i in range(n))

}

◊Verbatim[#:option "fontsize=\\scriptsize" #:verbatim "true"]{
  struct parse_instruction_and_construct_ast_p :
    pegtl::sor<
      inst_p<ret_keyword_p, make_new<ret_i_t>, 0>, // return
      inst_p<pegtl::seq<TAO_PEGTL_KEYWORD("br"),   //br
                   sep_p, save_p<label_ap>, sep_p>,
             make_new<br_i_t>, 1, 0>,
      ...

}

}

◊;{
- 研究高層次抽象化本身 卻依賴於低層次的抽象化，甚至依賴特定實做
- 不同層次的抽象化重點相異，低層次抽象化難以解決高層次問題
  低階語言多引入不必要的細節與限制
- 難以分析比較抽象化本身的設計與取捨
- 不同語言特性之間的互動
- 編譯成低階語言：若討論原始高階語言特性的互動，
  現在變成要討論 “高些語言程式在低階語言中的 image” 之間的互動。
}
◊frame{
◊frametitle{常見描述、定義程式語言特性的方式各有限制}

1. 用自然語言描述輔以範例。然而文字敘述無法操作、分析性質與研究語言構造的互動，
    並且面對複雜的構造有其極限。

2. 編譯成低階語言做為解釋。但可能引入不必要的細節或打破高階語言的抽象介面。

[Python 3 documentation, 6.2.9. Yield expressions](https://docs.python.org/3/reference/expressions.html\\#yieldexpr):
◊parbox[#:param "\\textwidth"]{◊braced{◊scriptsize[]
◊blockquote{
  When a generator function is called, it returns an iterator known
  as a generator. That generator then controls the execution of
  the generator function. The execution starts when one of the
  generator's methods is called. At that time, the execution
  proceeds to the first yield expression, where it is suspended
  again, returning the value of `expression\_list` to the generator's
  caller. By suspended, we mean that all local state is retained,
  including the current bindings of local variables, the instruction
  pointer, the internal evaluation stack, and the state of any
  exception handling. When the execution is resumed by calling
  one of the generator's methods, the function can proceed exactly
  as if the yield expression were just another external call.
}}}

} ◊;'unused-for-code-highlighting

◊;{
- 需要可以操作的物件協助設計工具
- e.g. 變數重命名 regular expression 搜尋取代
- 編譯器的最佳化 靜態分析工具
}
◊;{
- 所有程式語言都一樣，只是工具，演算法與資料結構才是核心
- 這個功能讓程式語言更有表達力
- 這個功能是不安全的
- 寫出錯誤是程式設計師的錯
- 語言特性間的互動: gradual typing and parametricity,
  statics and dynamics, issues with type-directed translation
}
◊frame{
◊frametitle{◊strong{程式語言理論}使用數學工具及形式邏輯建立範式與框架}

◊strong{可操作的規格與工具：}
藉由數學與形式邏輯工具，程式語言各個構造皆使用
數學精確定義，甚至一部份存在對應的數學物件。

同樣的，編譯、最佳化、重構變換、動態/靜態分析與驗證等工具也能在此
框架中以數學描述。我們並可以討論這些工具相對於
程式語言的定義是否正確。

◊strong{證明性質：}
透過程式語言構造的數學定義，我們能證明語言的設計
是否滿足所需的性質。

◊strong{分析優缺點：}
程式語言理論也包括分析語言的表達力強弱、探討
語言構造之間的互動關係。一個例子是說明為何
圖靈完備性並不等價於與程式語言的表達力。 ◊;{刻畫計算能力的}

}

◊frame{
◊frametitle{怎麼分析程式語言？}

**問題：**

- 程式語言表層語法對分析本身無關緊要
- 基於低層次抽象的描述過於瑣碎
- 自然語言的表述無法操作、計算

**方法：** 建立數學模型並以數學工具分析

**目標：**

- 簡潔描述程式語言中的程式
- 以數學模型精確定義程式的語義
- 形式的討論語言中不同部件的規格，例如型別系統
- 做到符號計算與推論
}

# 程式語言的數學模型

◊(define syntax-descs
   (list
    ◊@{一個語言中，程式通常以歸納定義的◊strong{抽象語法樹}建模。
       抽象語法樹略去了諸如排版、分號或括弧等語法分析用的細節，
       僅保留語言構造本身的結構。}
    ◊@{程式語言模型討論的對象物件即所有抽象語法樹的集合。}))

◊(define semantics-desc
   ◊@{抽象而言，語義指定了程式（抽象語法樹）的「意思」。
      語義有三種常見形式，分別從不同角度將「意思」詮釋成可以
      操作、推理的定義。
      舉例來說，操作語義是定義於程式集合之上的轉移系統。})

◊frame{
◊frametitle{程式語言的數學模型}

程式語言的定義常見分為語法、語義、可選的型別系統等部件。

1. **語法**：◊(list-ref syntax-descs 0)

    ◊(list-ref syntax-descs 1)

2. **語義**：◊semantics-desc

3. **型別系統**：
    型別系統為每一個子程式關聯了一些型別。
    整個程式關聯的型別並預測了其執行期間的行為。
    換句話說，型別系統相對於程式語言的語義是◊strong{健全} (*sound*) 的。

}

## 語法

◊frame[#:option "fragile"]{
◊frametitle{算術語言的語法：怎麼用◊strong{可操作}的方式建立程式的模型？}

◊block[#:param "回顧"]{
  ◊(list-ref syntax-descs 0)
}

考慮一個處理基礎算術的程式語言：◊strong{算術語言}。
◊arith-lang-defn-informal

◊vspace{0.5em}
◊example[#:option "具體語法 (concrete syntax)"]{
  ◊minipage[#:option "t" #:param "\\textwidth"]{
    ◊minipage[#:param "0.3\\textwidth"]{
      ◊center{◊Verbatim[#:option "frame=single,framesep=2mm" #:verbatim "true"]{
        (5 + 2)*3


      }}
    }
    ◊minipage[#:param "0.29\\textwidth"]{
      ◊center{◊Verbatim[#:option "frame=single,framesep=2mm" #:verbatim "true"]{
        3 +  4 *
            5

      }}
    }
    ◊minipage[#:param "0.36\\textwidth"]{
      ◊center{◊Verbatim[#:option "frame=single,framesep=2mm" #:verbatim "true"]{
        2 * 3 * (5 * 7)


      }}
    }
  }
}

}

◊frame{
◊frametitle{算術語言的語法：抽象語法樹}

算術語言的抽象語法樹包含：

1. 一種 terminal node：代表自然數的節點 (a) （◊${n ◊in[] ◊bbN}）。
1. 兩種 inner nodes：代表二元運算 `+`、`*` 的節點 (b) 與 (c)。

◊vspace{-0.3em}
◊(arith-lang-defn-ast)

◊vspace{-0.8em}
我們仍將以上的樹簡寫為數學算式，依照慣例省略括弧，但了解
**實際操作的對象是簡寫代指的語法樹**，
而非線性的符號串。

◊example[#:option "抽象語法樹 (abstract syntax tree) 的簡寫"]{
  ◊minipage[#:option "t" #:param "\\textwidth"]{
    ◊minipage[#:param "0.3\\textwidth"]{
      ◊${(5◊ttplus 2)◊tttimes 3}
    }
    ◊minipage[#:param "0.29\\textwidth"]{
      ◊${3◊ttplus (4◊tttimes 5)} ◊br
      或 ◊${3◊ttplus 4◊tttimes 5}
    }
    ◊minipage[#:param "0.36\\textwidth"]{
      ◊${((2◊tttimes 3)◊tttimes(5◊tttimes 7))} ◊br
      或 ◊${2◊tttimes 3◊tttimes (5◊tttimes 7)}
    }
  }
}

}

◊frame[#:option "fragile"]{
◊frametitle{簡單的 Haskell 實做}

```
data Expr = ILit Integer              -- (a)
  | Plus Expr Expr | Times Expr Expr  -- (b)(c)
```

◊(arith-lang-defn-ast)

◊example[#:option "以 algebraic data type \\texttt{Expr} 實作抽象語法樹"]{
  ◊Verbatim[#:option "fontsize=\\scriptsize,commandchars=\\\\\\{\\},xleftmargin=2mm,xrightmargin=2mm"
            #:verbatim "true"]{
  Times (Plus (ILit 5) (ILit 2)) (ILit 3)
  \hrulefill
  Plus (ILit 3) (Times (ILit 4) (ILit 5))
  \hrulefill
  Times (Times (ILit 2) (ILit 3))
        (Times (ILit 5) (ILit 7))

  }
}
}

◊frame{
◊frametitle{算術語言的語法：形式定義}

◊vspace{-1.5em}
◊(arith-lang-defn)

◊vspace{-0.5em}
抽象語法樹的算式簡寫常用 context-free grammar 歸納地定義。

◊vspace{-1.5em}
◊(arith-lang-defn-ast #:with-e? #t)

◊vspace{-1.5em}
◊${◊Term} 為所有抽象語法樹的集合。
於 ◊${◊grammar-rule} 左側，◊${◊Term ◊ni[] e}
表明符號 ◊${e} 代稱 ◊${◊Term} 中的語法樹。等號右側
由 ◊${◊grammar-sep} 分隔的 productions
依序對應語法樹 ◊${e} 的三種構造方法：
(a) ◊${n} (b) ◊${e◊ttplus e} 或 (c) ◊${e◊tttimes e}。

production 中的符號如 ◊${n}、◊${e} 代表對應子樹的類別，
例如 ◊${e◊ttplus e} 意指 (b) 中兩側的子樹都必須是
◊${◊Term} 中的語法樹。

}

## 語義

◊frame{
◊frametitle{語義：怎麼用◊strong{可操作}的方式定義程式的「意思」？}

◊block[#:param "回顧"]{◊semantics-desc}

從不同角度對「意思」作出的詮釋對應三種類型的語義：

1. **操作語義** (_operational semantics_)：

    程式怎麼執行？

2. **公理語義** (_axiomatic semantics_)：

    程式執行前後系統的預期狀態、性質是（以邏輯式表達）？

3. **指稱語義** (_denotational semantics_)：

    程式代表什麼意思？
}

◊frame[#:option "fragile"]{
◊frametitle{語義}

◊Verbatim[#:option "numbers=left,xleftmargin=10mm" #:verbatim "true"]{
int maximum(int a, int b) {
  if (a >= b)
    return a;
  return b;
}

}

* **程式怎麼執行？**

    `maximum` 被呼叫時將執行第 2 行。若 `a >= b` 是 `true`
    則執行第 3 行回傳 `a`，否則執行第四行回傳 `b`。

* **程式執行前後系統的預期狀態、性質是？**

    `maximum` 接收兩個參數，並回傳其中的較大值。

* **程式代表什麼意思？**

    `maximum(a,b)` 計算數學上的 ◊${◊lmax{(a,b)}} 函數。
}

◊frame[#:option "fragile"]{
◊frametitle{語義}

◊Verbatim[#:option "numbers=left,xleftmargin=10mm" #:verbatim "true"]{
int factorial(int n) {
  int prod = 1;
  while (n != 0) { prod = prod * n
                 ; n = n - 1; }
  return prod;
}

}

- **程式怎麼執行？** （略）

- **程式執行前後系統的預期狀態、性質是？**

    設 `factorial` 輸入 ◊${◊mathtt{n} := N}。當
    ◊${N ◊ge[] 0} 時，函式 `factorial` 會中止。其回傳值
    ◊${◊mathtt{factorial(n)} = N!}

- **程式代表什麼意思？**

    `factorial` 計算如右的函數：
    ◊${
      f(n) = ◊bigparens[#:left "\\{" #:right "."]{
      ◊array[#:param "ll"]{
        n! & ◊text{if }◊sp n ◊ge[] 0 ◊br
        ◊bot[] & ◊text{otherwise}
      }
    }}
}

◊frame{
◊frametitle{算術語言的 operational semantics}

結構操作語義 (*structural operational semantics*)
由 G. D. Plotkin 系統地整理，特性為

1. 以◊strong{轉移系統}的方式在語法樹集合上規範程式如何執行
2. 轉移系統本身以◊strong{語法導向}的原則◊strong{歸納}地定義。

將結構操作語義應用於算術語言時，我們引入幾個定義及概念：

- 一個語法樹集合的子集 ◊${◊Value ◊subset[] ◊Term}，包含所有最終的計算結果。
    集合中的元素 ◊${v ◊in[] ◊Value} 稱為◊strong{值}。 

- 一個定義於語法樹集合 ◊${◊Term} 上的 binary relation
    ◊${◊mreduce ◊sp ◊subset[] ◊Term ◊times[] ◊Term}，
    描述算術語言的程式如何◊strong{化簡}。

- 一個程式的意思是其經由 ◊${◊mreduce} 多次化簡最終得到的值。
    對 ◊${e ◊in[] ◊Term}，若
    ◊${e = e_1 ◊mreduce ◊dots[] ◊mreduce e_n = v}
    則 ◊${e} 的意思是 ◊${v}。

}

◊frame{
◊frametitle{算術語言的 operational semantics}

◊vspace{-1.5em}
◊(arith-lang-defn #:value? 'alert #:nat? #f)

◊vspace{-0.5em}
算術語言的值（最終計算結果）只有自然數一種可能，
而化簡操作恰巧對應到算術中的計算、化簡動作。

◊vspace{-0.5em}

- ◊${(n_1◊ttplus n_2) ◊mreduce n} 其中 ◊${n = n_1+n_2}
- ◊${(n_1◊tttimes n_2) ◊mreduce n} 其中 ◊${n = n_1◊times[] n_2}
- 計算化簡可能發生於程式中的一個子片段，如範例中劃線之處。
    因此 ◊${◊mreduce} 也擔負定位子片段、指定化簡順序的責任。

◊example{
  - ◊${◊underline{(5◊ttplus 2)}◊tttimes 3
       ◊mreduce ◊underline{◊mathbf{◊alert{7}}◊tttimes 3}
       ◊mreduce ◊mathbf{◊alert{21}}}
  - ◊${◊underline{2◊tttimes 3}◊tttimes (5◊tttimes 7)
       ◊mreduce ◊mathbf{◊alert{6}}◊tttimes ◊underline{(5◊tttimes 7)}
       ◊mreduce ◊underline{6◊tttimes ◊mathbf{◊alert{35}}}
       ◊mreduce ◊mathbf{◊alert{210}}}
}

}

◊frame{
◊frametitle{算術語言的 operational semantics}

◊vspace{-1.5em}
◊(arith-lang-defn #:value? #t #:nat? #f)

◊vspace{-0.5em}
我們能將 ◊${◊mreduce} 拆成◊strong{定位子片段}以及◊strong{定義實際化簡操作}兩部份。

實際參與化簡的算式如 ◊${n_1◊ttplus n_2}、◊${n_1◊tttimes n_2} 等
皆稱作 *redex*。令 binary relation
◊${◊mredex ◊subset[] ◊Term ◊times[] ◊Term}
負責定義 redexes 的化簡，則

◊vspace{-1.5em}
◊arith-lang-redexes

◊vspace{-0.5em}
至於在完整程式中定位子片段（即 redex），由於算術語言計算順序為由左至右，
因此 ◊${◊mreduce} 應該額外滿足

- ◊${◊mredex ◊subset[]◊sp ◊mreduce}，因算式本身可能是 redex
- ◊${e_1◊ttplus e_2 ◊mreduce e'_1◊ttplus e_2} 當 ◊${e_1◊mreduce e'_1}； ◊SP
    ◊${v ◊ttplus e_2 ◊mreduce v ◊ttplus e'_2} 當 ◊${e_2◊mreduce e'_2}。
- ◊${e_1◊tttimes e_2 ◊mreduce e'_1◊tttimes e_2} 當 ◊${e_1◊mreduce e'_1}； ◊SP
    ◊${v ◊tttimes e_2 ◊mreduce v ◊tttimes e'_2} 當 ◊${e_2◊mreduce e'_2}。

}

◊frame[#:option "fragile"]{
◊frametitle{算術語言的 operational semantics}

◊vspace{-1.5em}
◊arith-lang-redexes

◊vspace{-0.5em}
注意此處符號與語法定義的 production 意義相異。以下敘述中符號
◊${e_1}、◊${e_2}、◊${v} 皆指稱具體但未知、隸屬於指定類別的語法樹，
因此重複的符號代指相同的語法樹。
◊vspace{-0.5em}

- ◊${◊mredex ◊subset[]◊sp ◊mreduce}
- ◊${e_1◊ttplus e_2 ◊mreduce e'_1◊ttplus e_2} 當 ◊${e_1◊mreduce e'_1}； ◊SP
    ◊${v ◊ttplus e_2 ◊mreduce v ◊ttplus e'_2} 當 ◊${e_2◊mreduce e'_2}。
- ◊${e_1◊tttimes e_2 ◊mreduce e'_1◊tttimes e_2} 當 ◊${e_1◊mreduce e'_1}； ◊SP
    ◊${v ◊tttimes e_2 ◊mreduce v ◊tttimes e'_2} 當 ◊${e_2◊mreduce e'_2}。

◊example{
  ◊${◊array[#:param "lll"]{
    \underline{2◊tttimes 3}◊tttimes (5◊tttimes 7) ◊mreduce
      & ◊text{by}
      & (e_1◊tttimes e_2)◊mreduce (e'_1◊tttimes e_2) ◊land[]
        e_1◊mreduce e'_1
      ◊br
    ◊mathbf{◊alert{6}} ◊tttimes ◊underline{(5◊tttimes 7)} ◊mreduce
      & ◊text{by}
      & (v ◊tttimes e_2)◊mreduce (v ◊tttimes e'_2) ◊land[]
        e_2◊mreduce e'_2
      ◊br
    ◊underline{6 ◊tttimes ◊mathbf{◊alert{35}}} ◊mreduce
      & ◊text{by}
      & (n_1◊tttimes n_2)◊mredex n ◊land[]
        n=n_1◊times[] n_2
      ◊br
    ◊mathbf{◊alert{210}}
  }}
}

}

◊frame[#:option "fragile"]{
◊frametitle{算術語言的 operational semantics}

◊vspace{-1.5em}
◊arith-lang-redexes

我們用函數 `redex` 實作 relation
◊${◊mredex ◊subset[] ◊Term ◊times[] ◊Term}。

一般而言，對單一 ◊${e◊in[] ◊Term} 可能有複數的 ◊${e'◊in[] ◊Term}
滿足 ◊${e ◊mredex e'}，因此我們選擇用型別為 `Expr -> [Expr]`
的函數表達 ◊${◊mredex}。


```
redex :: Expr -> [Expr]
redex (Plus (ILit n) (ILit m)) = [ILit (n + m)]
redex (Times(ILit n) (ILit m)) = [ILit (n * m)]
redex _                        = []

```
}

◊frame[#:option "fragile"]{
◊frametitle{算術語言的 operational semantics}

函數 `reduce` 實作了 ◊${◊mreduce}。
`isValue` 負責甄別出 `Expr` 中的值。

◊vspace{-0.5em}

```
reduce :: Expr -> [Expr]
reduce e = redex e ++
  case e of
    ILit _      -> []
    Plus e1 e2  ->
      [ Plus e1' e2  | e1' <- reduce e1 ] ++
      [ Plus e1  e2' | isValue e1,
                       e2' <- reduce e2 ]
    Times e1 e2 ->
      [ Times e1' e2  | e1' <- reduce e1 ] ++
      [ Times e1  e2' | isValue e1,
                        e2' <- reduce e2 ]

```
}

◊frame{
◊frametitle{關於◊strong{可操作性}}

- 抽象語法樹的集合 ◊${◊Term} 是◊strong{歸納地}建構的；
    任意一個程式 ◊${e◊in[] ◊Term} 只有 ◊${e:=n◊in[] ◊bbN}、
    ◊${e:=e_1◊ttplus e_2} 或 ◊${e:=e_1◊tttimes e_2}
    等三種可能之一。

    由於 ◊${◊Term} 是歸納定義的，
    對程式的分析與操作可以轉化為 ◊${e◊in[] ◊Term}
    上的◊strong{遞迴定義}或◊strong{結構歸納法}。

- 雖然我們省略了 ◊${◊mreduce} 的形式定義，但 `reduce` 函數
    大略指出了 ◊${◊mreduce} 的歸納結構及操作方法。

    對於任意一步化簡操作 ◊${e ◊mreduce e'} 而言，
    共有以下的可能：

    * ◊${e ◊mreduce e'} 即為 ◊${e ◊mredex e'}，
        而 ◊${e ◊mredex e'} 又會是
        ◊${(n_1◊ttplus n_2)◊mredex (n_1+ n_2)} 或
        ◊${(n_1◊tttimes n_2)◊mredex (n_1◊times[] n_2)}
        之一。
    * ◊${e ◊mreduce e'} 即為 ◊${e_1◊ttplus e_2 ◊mreduce e'_1◊ttplus e_2}，
        同時 ◊${e_1◊mreduce e'_1} 。
    * ◊${e ◊mreduce e'} 即為 ◊${v◊ttplus e_2 ◊mreduce v◊ttplus e'_2}，
        同時 ◊${e_2◊mreduce e'_2} 。
    * 乘法的可能性。與加法類似。

}

◊frame{
◊frametitle{小節}

- 程式語言基本的數學模型包含◊strong{語法}及◊strong{語義}兩部份。

- 一個語言的語法用◊strong{抽象語法樹}建模。抽象語法樹的集合即為所有的程式。

- 語義中的結構操作語義為語法樹集合上的◊strong{轉移系統}，
    以值的集合搭配語法樹集合上的 binary relation 描述。

◊block[#:param "Side Note"]{
  結構操作語義中描述轉移的 relation 事實上可以用
  inference rules 以及 derivations 正式地、結構地
  書寫。如此能清晰的表述該 relation 的歸納結構
  （諸如歸納假設等等）。
}

}

## 擴展算術語言：變數綁定及語法作用域

◊frame[#:option "fragile"]{
◊frametitle{加入變數}

◊vspace{-1.5em}
◊arith-var-lang-defn

◊vspace{-0.5em}
我們在算術語言中引入程式語言中常見的變數構造。
新的語言構造有變數參考以及建立新綁定的 let 算式兩種。

算式 ◊${◊(arith-let ◊@{x} ◊@{e_1} ◊@{e_2})} 將會先計算 ◊${e_1}
的值，並在計算 ◊${e_2} 時把 ◊${x} 綁定為 ◊${e_1} 計算的結果。

◊example{
  ◊minipage[#:option "t" #:param "\\textwidth"]{
    ◊minipage[#:param "0.22\\textwidth"]{
      ◊${1+◊aid{s}}
    }
    ◊minipage[#:param "0.33\\textwidth"]{
      ◊${◊array[#:param "l"]{
        ◊(arith-let
          ◊aid{z} ◊@{5}
          ◊@{◊br
             ◊(arith-let
               ◊aid{w} ◊@{◊aid{z} ◊ttplus 3}
               ◊@{◊br
                  ◊aid{z}◊tttimes ◊aid{w}})})
      }}
    }
    ◊minipage[#:param "0.42\\textwidth"]{
      ◊${◊array[#:param "l"]{
        ◊(arith-let
          ◊aid{x} ◊@{2}
          ◊@{◊br
             (◊(arith-let
                ◊aid{y} ◊@{5}
                ◊@{◊aid{x}◊tttimes ◊aid{y}}))
             ◊ttplus ◊aid{x}})
      }}
    }
  }
}

}

◊frame{
◊frametitle{Binding、Scopes 與 Substitution}

◊vspace{-1.5em}
◊arith-var-lang-defn

◊vspace{-0.5em}
令符號 ◊${◊(subst ◊@{e} ◊@{e'} ◊@{x})} 代表 *capture-avoiding substitution*，
即在保持 let 綁定結構的情況下把 ◊${e} 中所有未綁定的變數 ◊${x}
都代換成 ◊${e'}。

算式 ◊${e} 中未綁定的變數稱為 ◊${e} 當中的◊strong{自由變數} (*free variable*)。

let 算式能透過 substitution 定義。
敘述「計算 ◊${e} 時將變數 ◊${x} 綁定到值 ◊${v}」
能翻譯為 ◊${◊(subst ◊@{e} ◊@{v} ◊@{x})}，
如此我們間接地描述了綁定的作用。

◊example{
  ◊${◊array[#:param "lcl"]{
    ◊(subst ◊@{(◊aid{x}◊ttplus ◊underline{◊aid{y}} ◊ttplus 5)}
            ◊@{8}
            ◊aid{y})
      & =
      & ◊aid{x}◊ttplus ◊underline{8} ◊ttplus 5
        ◊br
    ◊(subst ◊@{(◊underline{◊aid{z}}◊tttimes
                (◊(arith-let ◊mathbf{◊alert{◊aid{z}}} ◊@{2}
                             ◊@{◊mathbf{◊alert{◊aid{z}}}◊ttplus 3})))}
            ◊@{1}
            ◊aid{z})
      & =
      & ◊underline{1} ◊tttimes
        (◊(arith-let ◊mathtt{◊alert{◊aid{z}}} ◊@{2}
                     ◊@{◊mathtt{◊alert{◊aid{z}}}◊ttplus 3}))
  }}
}

}

◊frame{
◊frametitle{計算 let 算式}

◊vspace{-1.5em}
◊arith-var-lang-defn

◊vspace{-0.5em}
更正式的說，我們如以下方式擴展 binary relations
◊${◊mredex}、◊${◊mreduce} 來描述 let 算式與變數的語義：

- ◊${(◊(arith-let ◊@{x} ◊@{v} ◊@{e}))◊mredex (◊(subst ◊@{e} ◊@{v} ◊@{x}))}
- ◊${◊(arith-let ◊@{x} ◊@{e_1} ◊@{e_2}) ◊mreduce
   ◊(arith-let ◊@{x} ◊@{e'_1} ◊@{e_2})}
  當 ◊${e_1◊mreduce e'_1}

◊example{
  ◊${◊array[#:param "l"]{
    ◊underline{
      ◊(arith-let
        ◊aid{x} ◊@{2}
        ◊@{(◊(arith-let
              ◊aid{y} ◊@{3◊ttplus ◊aid{x}}
              ◊@{◊aid{x}◊tttimes ◊aid{y}}))
            ◊ttplus ◊aid{x}})}
      ◊mreduce
      ◊br
    (◊(arith-let
       ◊aid{y} ◊underline{3◊ttplus ◊mathbf{◊alert{2}}}
       ◊@{◊mathbf{◊alert{2}}◊tttimes ◊aid{y}}))
    ◊ttplus ◊mathbf{◊alert{2}}
      ◊mreduce
      ◊br
    ◊underline{
      (◊(arith-let
         ◊aid{y} ◊mathbf{◊alert{5}}
         ◊@{2◊tttimes ◊aid{y}}))}
    ◊ttplus 2
      ◊mreduce
      ◊br
    ◊underline{(2◊tttimes ◊mathbf{◊alert{5}})}
    ◊ttplus 2
      ◊mreduce
    ◊underline{
      ◊mathbf{◊alert{10}}
      ◊ttplus 2}
      ◊mreduce
    ◊mathbf{◊alert{12}}}
  }
}

}

## 擴展算術語言：更多內建資料型態以及型別系統

◊frame{
◊frametitle{加入其他資料型態}

◊vspace{-1.5em}
◊arith-bool-lang-defn

◊vspace{-0.5em}
我們往算術語言中引進 boolean 作為新的原生資料類別。
與 boolean 相關的新語言構造包含

- boolean 值 ◊${b ◊in[] ◊bbB = ◊lset{◊tt, ◊ff}}
- 判定兩個算式計算結果是否相等的 ◊${◊tteq} 運算子
- 有分支、選擇功能的 if 算式

我們並擴展最終計算結果 —— ◊${◊Value} 的定義，
納入 boolean 值。

◊example[#:option "程式片段"]{
  ◊${
    ◊(arith-if ◊@{◊aid{z} ◊tteq 0} ◊@{0} ◊@{◊aid{z} ◊tttimes 3})
  }
}

}

◊frame{
◊frametitle{boolean 值與 if 算式的計算}

注意由於 ◊${v ◊in[] ◊Value} 擴展了，原先 ◊${◊mreduce} 中的規則也有受到影響。
◊vspace{-0.5em}

- ◊${(v_1 ◊tteq v_2) ◊mredex ◊tt}，
    如果 ◊${v_1=v_2 ◊in[] ◊bbN} 或 ◊${v_1=v_2 ◊in[] ◊bbB} ◊br 
    ◊${(v_1 ◊tteq v_2) ◊mredex ◊ff}，其他情況
- ◊${e_1 ◊tteq e_2 ◊mreduce e'_1 ◊tteq e_2} 當 ◊${e_1 ◊mreduce e'_1}； ◊SP
    ◊${v ◊tteq e_2 ◊mreduce v ◊tteq e'_2} 當 ◊${e_2 ◊mreduce e'_2}

- ◊${(◊(arith-if ◊tt ◊@{e_1} ◊@{e_2})) ◊mredex e_1} ◊br
    ◊${(◊(arith-if ◊ff ◊@{e_1} ◊@{e_2})) ◊mredex e_2}
- ◊${◊(arith-if ◊@{e} ◊@{e_1} ◊@{e_2}) ◊mreduce
     ◊(arith-if ◊@{e'} ◊@{e_1} ◊@{e_2})}
  當 ◊${e ◊mreduce e'}

◊example{
  ◊${
    ◊(arith-if ◊@{(◊(arith-if ◊underline{5 ◊tteq 3} ◊ff ◊tt))}
               ◊@{3 ◊ttplus 4}
               ◊@{0})
      ◊mreduce
    ◊(arith-if ◊underline{(◊(arith-if ◊mathbf{◊alert{◊ff}} ◊ff ◊tt))}
               ◊@{3 ◊ttplus 4}
               ◊@{0})
      ◊mreduce
    ◊underline{
      ◊(arith-if ◊mathbf{◊alert{◊tt}}
                 ◊@{3 ◊ttplus 4}
                 ◊@{0})}
      ◊mreduce
    ◊underline{◊mathbf{◊alert{3 ◊ttplus 4}}}
      ◊mreduce
    ◊mathbf{◊alert{12}}
    }
    ◊br
  ◊vspace{0.5em}
  ◊${
    (5 ◊tttimes ◊underline{(8 ◊tteq ◊tt)}) ◊tteq 0
      ◊mreduce
    (5 ◊tttimes ◊mathbf{◊alert{◊ff}}) ◊tteq 0
      ◊lnot ◊mreduce} (*stuck*!)
}

}

◊(define intro-type-motivation
  ◊@{
    進一步延伸模型，納入◊strong{型別}的概念。
    型別是什麼？有什麼作用？
  })

◊frame[#:option "fragile"]{
◊frametitle{型別與型別系統}

◊intro-type-motivation

```
bool has_solution(int a, long b, int c) {
  long AC = 4 * (long)a * (long)c;
  return b*b >= AC;
}
```

對型別字面上的理解很容易得到下列不完整的印象：

- 型別代表儲存一個變數所需的記憶體空間
- 型別代表一個值在記憶體中儲存的形式
- 型別代表一個變數被允許的操作
}

◊frame{
◊frametitle{型別與型別系統}

◊intro-type-motivation

◊braced{◊small{
  J.H. Morris, _Lambda-Calculus Models of Programming Languages_,
  PhD dissertation, pp. 12, 1968:
}}

◊parbox[#:param "\\textwidth"]{
> ``We tend to understand these subjects pragmatically.
> When a programner thinks of recursion, he thinks of
> push-down stacks and other aspects of how recursion
> "works". Similarly, types and type declarations are
> often described as communications to a compiler to
> aid it in allocating storage, etc.
>
> The *thesis* of this dissertation, then, is that these
> aspects of programming languages can be given an intuitively
> reasonable semantic interpretation.''
}
}

◊frame{
◊frametitle{型別與型別系統}

◊intro-type-motivation

◊braced{◊small{
  B.C. Pierce, _Types and Programming Languages_,
  pp. 1-13:
}}

◊parbox[#:param "\\textwidth"]{
> ``A type system is a tractable syntactic method for
> proving the absence of certain program behaviors
> by classifying phrases according to the kinds of
> values they compute.''
}

型別以及對應的型別系統

- 是◊strong{基於語法}的◊strong{靜態}系統
- 對於◊strong{程式的每一個片段}，預測、分類其◊strong{執行結果}。
- 因為預測了執行結果，所以能提前避免各式錯誤
}

◊frame{
◊frametitle{算術語言的型別系統}

◊vspace{-1.5em}
◊(arith-type-defn #:show-Γ? #t)

◊vspace{-0.5em}
算術語言的型別系統是一個自然演繹
風格的◊strong{證明演算系統}，對每個程式片段做出◊strong{判斷} (*judgment*)，
並分類為適當的型別，最終整合所有判斷來證明完整程式的執行結果。

此系統的型別有 ◊${◊bbN} 與 ◊${◊bbB} 二種，
做出的 judgment 形式為
◊$${◊(:-types ◊boldsymbol{◊alert{◊GG}}
               ◊boldsymbol{◊alert{e}}
               ◊boldsymbol{◊alert{◊Gt}})}
意指程式片段 ◊${e} 在 ◊${◊GG} 的環境下型別為 ◊${◊Gt}
（符號「◊${◊vdash[]}」、「◊${:}」是名字的一部分）。
各個的 judgments 透過如下示意的 *inference rules* 
組織並形成成 *derivation*。
◊vspace{-0.5em}

◊prooftree{
  ◊(infer$ 
    (infer$ (:-types ◊@{◊GG_1} ◊@{e_1} ◊@{◊Gt_1}))
    (infer$ ◊dots[])
    (infer$ (:-types ◊@{◊GG_n} ◊@{e_n} ◊@{◊Gt_n}))
    #:label ◊rule{Name}
    #:-----
    (:-types ◊GG ◊@{e} ◊@{◊Gt}))
}

◊vspace{-0.5em}
我們暫時延後環境 ◊${◊GG} 的解釋； ◊${◊GG} 與變數相關。

}

◊frame{
◊frametitle{插曲：自然演繹 (natural deduction)}

自然演繹沒有限定 judgment 的格式。
在 ◊${◊(:-types GG ◊@{e} ◊Gt)} 之外，
抽象語法樹、◊${◊mreduce} 與 ◊${◊mredex}
皆能用相同方法定義。而架構上 judgments 都是透過 inference rules
進行組織的：

◊vspace{-0.5em}
◊prooftree{
  ◊(infer$
    (infer$ ◊@{J_1})
    (infer$ (dots))
    (infer$ ◊@{J_n})
    #:label ◊rule{Name}
    #:-----
    ◊@{J})
}

◊vspace{-0.5em}
在此 inference rule 中，◊${J_1,◊dots[],J_n}
是其◊strong{前提} (*premise*)，◊${J} 是其◊strong{結論} (*conclusion*)。
透過重複應用 inference rules，judgments 會組合成樹狀的 derivation：

◊vspace{-2em}
◊prooftree{
  ◊(infer$
    (infer$ (infer$ (infer$ (vdots))
                    #:-----
                    ◊@{J_◊braced{1,1}})
            (infer$ (dots))
            (infer$ #:-----
                    ◊@{J_◊braced{1,m_1}})
            #:-----
            ◊@{J_1 ◊quad[] ◊dots[]})
    (infer$ (infer$ (infer$ #:-----
                            ◊@{J_◊braced{n,1,1}})
                    (infer$ (dots))
                    (infer$ (infer$ (vdots))
                            #:-----
                            ◊@{J_◊braced{n,1,k_◊braced{n1}}})
                    #:-----
                    ◊@{J_◊braced{n,1} ◊quad[] ◊dots[]})
            (infer$ #:-----
                    ◊@{◊dots[] ◊quad[] J_◊braced{n,m_n}})
            #:-----
            ◊@{◊dots[] ◊quad[] J_n})
    #:-----
    ◊@{J})
}

◊vspace{-0.5em}
derivation 起始的 judgment 必須沒有前提，即橫線上方為空。

}

◊frame{
◊frametitle{插曲：自然演繹 (natural deduction)}

◊minipage[#:option "t" #:param "\\textwidth"]{
  ◊minipage[#:param "0.42\\textwidth"]{
    ◊prooftree{
      ◊(infer$
        (infer$ ◊@{J_1 ◊SP ◊SP ◊dots[] ◊SP ◊SP J_n})
        #:label ◊rule{Name-A}
        #:-----
        ◊@{J})
    }
  }
  ◊minipage[#:param "0.4\\textwidth"]{
    ◊(prooftree
      (infer$
      (infer$ (:-types GG ◊@{e_1} bbN))
      (infer$ (:-types GG ◊@{e_2} bbN))
      #:label ◊rule{T-Plus}
      #:-----
      (:-types GG ◊@{e_1 ◊ttplus e_2} bbN)))
  }
}

inference rule
捕捉了◊strong{單步邏輯推導}背後的直覺。當前提
◊${J_1},...,◊${J_n} 皆有 derivations 時，
◊rule{Name-A} 表明我們能據此判斷 ◊${J} 成立，
同時藉由 ◊rule{Name-A} 將 ◊${J_1,◊dots[],J_n} 的
derivations 組合作成 ◊${J} 的 derivation。

◊vspace{-1.5em}
◊center{
  ◊tikzpicture[
    #:option
    ◊string-append{
      deriv/.style={
        isosceles triangle,
        isosceles triangle apex angle=40,
        shape border rotate=-90,
        minimum width=0.8cm
      }
    }]{◊raw-latex{
      \node at (-2.4cm, 0cm) {$J_1$} [grow'=up]
        child { node[deriv,yshift=-0.9cm,draw] {}
                edge from parent[draw=none] }
        ;
        \node at (-2.7cm, 0.9cm) {$\overline{\; J \;}$};
        \node at (-2.1cm, 0.9cm) {$\overline{\; J' \;}$};  ◊;{'comment}
      \node at (-1.6cm,0.3cm) {$\dots$};
      \node at (-0.8cm, 0cm) {$J_n$} [grow'=up]
        child { node[deriv,yshift=-0.9cm,draw] {}
                edge from parent[draw=none] }
        ;
        \node at (-0.8cm, 0.9cm) {$\overline{\; J'' \;}$};
      \node at (0.5cm,0.3cm) {$\Longrightarrow$};
      \node at (1.8cm, 0.3cm) {$J_1$} [grow'=up]
        child { node[deriv,yshift=-0.9cm,draw] {}
                edge from parent[draw=none] }
        ;
        \node at (1.5cm, 1.2cm) {$\overline{\; J \;}$};
        \node at (2.1cm, 1.2cm) {$\overline{\; J' \;}$};  ◊;{'comment}
      \node at (2.6cm,0.6cm) {$\dots$};
      \node at (3.4cm, 0.3cm) {$J_n$} [grow'=up]
        child { node[deriv,yshift=-0.9cm,draw] {}
                edge from parent[draw=none] }
        ;
        \node at (3.4cm, 1.2cm) {$\overline{\; J'' \;}$};
      \draw (1.5cm,0cm) -- (3.7cm,0cm);
      \node at (2.6cm,-0.2cm) {$J$};
      \node at (4.5cm,0cm) {\textsc{[Name-A]}};
    }
  }
  ◊vspace{-1.2em}
}

應用於型別系統時，◊rule{T-Plus} 描述如何判斷
片段 ◊${e_1 ◊ttplus e_2} 是否具有型別 ◊${◊bbN}。
若存在 derivations 證明 ◊${e_1}、◊${e_2}
在環境 ◊${◊GG} 中皆有型別 ◊${◊bbN}，
◊rule{T-Plus} 便能推導出 ◊${e_1 ◊ttplus e_2} 在
◊${◊GG} 下同樣具有型別 ◊${◊bbN}。

◊;{易見 ◊rule{T-Plus} 的設計符合了我們對 ◊${◊ttplus} 的認知。}

}

◊;{ terminal case, 不需要前提的 inference rule 自成 derivation }
◊;{◊frame{
◊frametitle{插曲：自然演繹 (natural deduction)}

◊minipage[#:option "t" #:param "\\textwidth"]{
  ◊minipage[#:param "0.5\\textwidth"]{
    ◊prooftree{
      ◊(infer$
        #:label ◊rule{Name-B}
        #:-----
        ◊@{J})
    }
  }
  ◊minipage[#:param "0.4\\textwidth"]{
    ◊(prooftree
      (infer$
        #:label ◊rule{T-Nat}
        #:-----
        (:-types GG ◊@{n} bbN)))
  }
}

一個 derivation
}}

◊frame{
◊frametitle{算術語言的型別系統：Inference Rules}
◊minipage[#:option "t" #:param "\\textwidth"]{
  ◊minipage[#:param "0.35\\textwidth"]{
    ◊(prooftree
      (infer$
       #:label ◊rule{T-Nat}
       #:-----
       (:-types GG ◊@{n} bbN)))
  }
  ◊minipage[#:param "0.65\\textwidth"]{
    ◊(prooftree
      (infer$
       (infer$ (:-types GG ◊@{e_1} bbN))
       (infer$ (:-types GG ◊@{e_2} bbN))
       #:label ◊rule{T-Plus}
       #:-----
       (:-types GG ◊@{e_1 ◊ttplus e_2} bbN)))
    ◊;
    ◊(prooftree
      (infer$
       (infer$ (:-types GG ◊@{e_1} bbN))
       (infer$ (:-types GG ◊@{e_2} bbN))
       #:label ◊rule{T-Times}
       #:-----
       (:-types GG ◊@{e_1 ◊tttimes e_2} bbN)))
  }
}
◊;
◊example{
  ◊vspace{-1em}
  ◊(prooftree
    (infer$
     (infer$
      (infer$ #:label ◊rule{T-Nat}
              #:-----
              (:-types "" ◊@{1} bbN))
      (infer$ #:label ◊rule{T-Nat}
              #:-----
              (:-types "" ◊@{2} bbN))
      #:label ◊rule{T-Plus}
      #:-----
      (:-types "" ◊@{1◊ttplus 2} bbN))
     (infer$ #:label ◊rule{T-Nat}
             #:-----
             (:-types "" ◊@{7} bbN))
     #:label ◊rule{T-Times}
     #:-----
     (:-types "" ◊@{(1◊ttplus 2)◊tttimes 7} bbN)))
  ◊vspace{-1em}
}

◊vspace{-0.5em}
◊textit{Remark.} ◊sp
此處與操作語義類似，符號 ◊${e_1}、◊${e_2} 等
指稱任意具體的語法樹。重複的符號代指相同的語法樹。

◊rule{T-Nat}、◊rule{T-Plus} 與 ◊rule{T-Times}
皆為樣板。他們界定如何對特定外型的算式
（ ◊${e_1 ◊ttplus e_2}、◊${e_1 ◊tttimes e_2}）進行證明推導，
能任意實例化。

}

◊frame{
◊frametitle{算術語言的型別系統：Inference Rules}

◊minipage[#:option "t" #:param "\\textwidth"]{
  ◊minipage[#:param "0.35\\textwidth"]{
    ◊(prooftree
      (infer$
      #:label ◊rule{T-Bool}
      #:-----
      (:-types GG ◊@{b} bbB)))
  }
  ◊minipage[#:param "0.65\\textwidth"]{
    ◊(prooftree
      (infer$
       (infer ◊${◊(:-types GG ◊@{e_1} ◊@{◊Gt_1})
                 ◊quad[]
                 ◊(:-types GG ◊@{e_2} ◊@{◊Gt_2})})
       #:label ◊rule{T-Equ}
       #:-----
       (:-types GG ◊@{e_1 ◊tteq e_2} bbB)))
  }
}
◊(prooftree
  (infer$
   (infer$ (:-types GG ◊@{e_1} bbB))
   (infer$ (:-types GG ◊@{e_2} ◊@{◊Gt}))
   (infer$ (:-types GG ◊@{e_3} ◊@{◊Gt}))
   #:label ◊rule{T-If}
   #:-----
   (:-types GG
            (arith-if ◊@{e_1} ◊@{e_2} ◊@{e_3})
            ◊@{◊Gt})))
◊;
◊(arith-type-defn)
◊vspace{-1.5em}
◊example{
  ◊vspace{-1em}
  ◊(prooftree
    (infer$
     (infer$
      (infer$ #:-----
              (:-types "" ◊@{7} bbN))
      (infer$ #:-----
              (:-types "" ◊@{0} bbN))
      #:-----
      (:-types "" ◊@{7◊tteq 0} bbB))
     (infer$ #:-----
             (:-types "" ff ◊alert{◊bbB}))
     (infer$ #:-----
             (:-types "" tt ◊alert{◊bbB}))
     #:-----
     (:-types "" (arith-if ◊@{7◊tteq 0} ff tt) ◊alert{◊bbB})))
}

由於無法預先知道 if 算式的選擇，◊rule{T-If}
要求 if 算式的兩個分支型別相同，以此確認 if 算式
的運算結果同樣為固定的型別。

}

◊frame[#:option "fragile"]{
◊frametitle{在 Judgment 中加入假設：Contexts}

型別系統對◊strong{每一個程式片段}都預測一個型別，那麼：

- 程式片段 `b*b >= AC` 的型別是什麼？

    ```
    bool has_solution(int a, long b, int c) {
      long AC = 4 * (long)a * (long)c;
      return b*b >= AC;
    }
    ```

- 怎麼準確的指定 ◊${◊aid{z} ◊tteq 0} 算式的型別？

    ◊vspace{-2em}
    ◊flalign*{
      ◊array[#:param "l"]{
        ◊arith-let[
          ◊aid{z}
          ◊@{3◊ttplus 5}
          ◊@{◊br
            ◊arith-if[
              ◊fbox{◊alert{◊${◊mathbf{◊aid{z} ◊tteq 0}}}}
              ff
              tt
            ]
          }
        ]
      }
      &&
    }

在型別 judgment ◊${◊(:-types GG ◊@{e} ◊Gt)} 當中，
◊${◊GG := (x_1 : ◊Gt_1), ◊dots[], (x_n : ◊Gt_n)}
為程式片段 ◊${e} 提供了其可見的變數綁定上下文。
◊(raw-latex "\n")

}

◊(define T-Var
   (prooftree
    (infer$
     #:label  ◊@{
                ◊braced{◊scriptsize{(◊${
                  x ◊unsp : ◊unsp ◊Gt ◊in[] ◊GG
                })}}
                ◊SP
                ◊rule{T-Var}
              }
     #:-----
     (:-types GG ◊@{x} ◊Gt))))

◊(define (T-Let #:color? [color? #f])
   (prooftree
    (infer$
     (infer$ (:-types GG
                      (if color?
                          ◊boldsymbol{◊alert{e_1}}
                          ◊@{e_1})
                      (if color?
                          ◊boldsymbol{◊alert{◊Gt_1}}
                          ◊@{◊Gt_1})
                      ))
     (infer$ (:-types ◊@{◊GG, ◊Sp
                         (◊(if color?
                               ◊boldsymbol{◊alert{x : ◊Gt_1}}
                               ◊@{x : ◊Gt_1}))}
                      ◊@{e_2} ◊@{◊Gt_2}))
     #:label ◊rule{T-Let}
     #:-----
     (:-types GG
              (if color?
                  (arith-let ◊boldsymbol{◊alert{x}}
                             ◊boldsymbol{◊alert{e_1}}
                             ◊@{e_2})
                  (arith-let ◊@{x}
                             ◊@{e_1}
                             ◊@{e_2}))
              ◊@{◊Gt_2}))))

◊frame{
◊frametitle{算術語言的型別系統：Inference Rules}

◊T-Var
◊vspace{-1em}
◊(T-Let #:color? #t)

◊vspace{-1em}
◊(arith-type-defn #:show-Γ? #t #:show-type? #f)

◊vspace{-0.5em}
對於變數參考，◊rule{T-Var} 直接從型別上下文 ◊${◊GG} 中
獲取變數的型別。
對於 let 算式，◊rule{T-Let} 將其動態語義轉譯成推導規則。

考慮算式 ◊${◊(arith-let ◊@{x} ◊@{e_1} ◊@{e_2})}。
此算式將計算 ◊${e_1} 的值，並將 ◊${x} 綁定到 ◊${e_1}
的計算結果後繼續計算 ◊${e_2}。

若 ◊${e_1} 在環境 ◊${◊GG} 下有型別 ◊${◊Gt_1}，
那麼 ◊${e_1} 的計算結果也將是型別為 ◊${◊Gt_1} 的值。
因此將 ◊${x} 的型別指定為 ◊${◊Gt_1} 的環境下，
◊${e_2} 具有的型別即為 let 算式的型別。

}

◊;{
  Caveat: ``那麼 ◊${e_1} 的計算結果也將是型別為 ◊${◊Gt_1} 的值。''
  IS THIS PROVED?

  作業：
  ◊${◊(arith-let ◊@{x} ◊@{v} ◊@{e}) ◊mreduce ◊(subst ◊@{e} ◊@{v} ◊@{x})}

◊(prooftree
  (infer$ (infer$ (:-types "" ◊@{v} ◊Gt))
          (infer$ (:-types ◊@{x : ◊Gt}
                           ◊@{e}
                           ◊@{◊Gt'}))
          #:-----
          (:-types ""
                   (arith-let ◊@{x} ◊@{v} ◊@{e})
                   ◊@{◊Gt'})))

◊(prooftree
  (infer$ (:-types "" (subst ◊@{e} ◊@{v} ◊@{x}) ◊@{◊Gt'}))) ◊;{'comment}

}

◊frame{
◊frametitle{算術語言的型別系統：Inference Rules}

◊T-Var
◊vspace{-1em}
◊(T-Let #:color? #f)
◊;
◊(let ()
  (define env ◊@{◊aid{z}:◊bbN})
  ◊example{
    令 ◊${e_◊text{not}:=(◊(arith-if ◊@{◊aid{z} ◊tteq 0} ff tt))}
    且令 ◊${D} 代表
    ◊vspace{-1em}
    ◊(prooftree
      (infer
       (infer #:label ◊textbf{◊alert{◊rule{T-Var}}}
              #:-----
               ($ (:-types env ◊aid{z} bbN)))
       (infer #:-----
               ($ (:-types env ◊@{0} bbN)))
       #:-----
       ($ (:-types env ◊@{◊aid{z} ◊tteq 0} bbB))))
    ◊vspace{-1em}
    則 ◊vspace{-1.5em}
    ◊(prooftree
      (infer$
       (infer$ #:-----
               (:-types "" ◊@{7} bbN))
       (infer$
        (infer$ ◊@{D})
        (infer$ #:-----
                (:-types env ff bbB))
        (infer$ #:-----
                (:-types env tt bbB))
        #:-----
        (:-types env ◊@{e_◊text{not}} bbB))
       #:label ◊textbf{◊alert{◊rule{T-Let}}}
       #:-----
       (:-types ""
                ◊(arith-let
                  ◊aid{z}
                  ◊@{7}
                  ◊@{e_◊text{not}})
                bbB)))
  })
}

# 結論

◊frame{
◊frametitle{程式語言理論研究的框架}

**數學模型**

我們以抽象語法樹及結構操作語義建立了程式語言的數學模型。
此數學模型形式化的定義了程式的語法及語義，因此允許對
程式本身的計算推理。

我們並介紹了如何擴展對象語言，加入變數相關的
語法構造及新的資料型別。在展示過的語法構造之外，
相同的框架能容納更多複雜的特性，包含函數、
遞迴（自我指涉）、複合型資料型態、控制流結構、
控制算子等等。

最後，我們概述了最簡單的靜態分析：型別系統。

**證明與分析**

由於我們介紹的數學模型皆為歸納定義的，
數學模型本身的性質（因此也是程式語言本身的性質）
能用結構歸納法加以證明。

}

◊;{
  應用:
  more (compound) datatypes
  type soundness
  program equality
  equational reasoning
  recursion meaning (self-reference)
}
