#lang racket

#|

CSC324 — 2024F — Assignment 2

Higher-Order Functions, Currying, and Evaluation

Tasks

1. Higher Order Functions:
  (a) map-expr
  (b) Functions using fold-expr:
    - expr-to-number
    - expr-to-list
2. Currying [Haskell only; See Haskell file for Task 2.]
3. Evaluation [Haskell only; See Haskell file for Task 3.]

All function bodies to implement are marked with "Complete me".

Recall that #; means "expression comment". Uncomment the tests as you complete the tasks. 

Note that by default, DrRacket only displays output for failing test cases. 
If you don't see any output, that means you've passed the tests. 

IMPORTANT: You are NOT allowed to modify existing imports or add new imports.

|#

(module+ test (require rackunit))
(provide Neg Plus Times map-expr expr-to-number expr-to-list)

#|
-------------------------------------------------------------------------------
* Task 1: Higher Order Functions *
-------------------------------------------------------------------------------

We define the minimal arithmetic expression language as follows:

An arithmetic expression is
- A number, or
- A `(Neg expr)`, where `expr` is the subexpression, or
- A `(Plus left right)`, where `left` and `right` are the subexpressions, or
- A `(Times left right)`, where `left` and `right` are the subexpressions.

We define the constructors `Neg`, `Plus`, and `Times` as Racket struct:
|#

(struct Neg (expr) #:transparent)
(struct Plus (left right) #:transparent)
(struct Times (left right) #:transparent)

#| 
(a) `map-expr`

Recall that we learned about `map` for lists in class:
|#

(module+ test
  (check-equal? (map - '(1 2 3)) '(-1 -2 -3))
  (check-equal? (map (λ (x) (+ x 3)) '(1 2 3)) '(4 5 6)))

#| 

Implement `map-expr` that maps expressions, which takes as input
  - a function `f` that takes a number as input and 
  - an expression `expr`,
and applies `f` to each number in `expr` (without changing the structure of `expr`).
|#

(module+ test
  (check-equal? (map-expr (λ (x) (+ (- x) 1)) 5) -4)
  (check-equal? (map-expr (λ (x) (+ (- x) 1)) (Neg 5)) (Neg -4))
  (check-equal? (map-expr (λ (x) (+ (- x) 1)) (Plus 3 (Neg 4))) (Plus -2 (Neg -3))))

(define (map-expr f expr)
  (let ([recurse (lambda (subexpr)
                   (map-expr f subexpr))])
    (match expr
      [(? number? a) (f a)]
      [(Neg e) (Neg (recurse e))]
      [(Plus l r) (Plus (recurse l) (recurse r))]
      [(Times l r) (Times (recurse l) (recurse r))])))

#|
(b) `fold-expr`

Recall that we learned about the higher-order functions `foldl` and `foldr` for lists in class:
|#

(module+ test
  (check-equal? (foldl + 0 '(1 2 3)) 6)
  (check-equal? (foldl * 1 '(1 2 3)) 6))

#|
You are given function `fold-expr` that "folds" expreesions, which takes as input
  - a function `f-num` (which describes how a number should be folded),
  - a function `f-neg` (which describes how `(Neg c)` should be folded),
  - a function `f-plus` (which describes how `(Plus l r)` should be folded),
  - a function `f-times` (which describes how `(Times l r)` should be folded), and
  - an expression `expr`.
Then, it folds the expression `expr` using the given functions.

`fold-expr` is implemented for you below. Read and understand the implementation and behaviour.
|#

(define (fold-expr f-num f-neg f-plus f-times expr)
  (let ([recurse (lambda (subexpr)
             (fold-expr f-num f-neg f-plus f-times subexpr))])
    (match expr
      [(? number? a) (f-num a)]
      [(Neg e) (f-neg (recurse e))]
      [(Plus l r) (f-plus (recurse l) (recurse r))]
      [(Times l r) (f-times (recurse l) (recurse r))])))

#|
Now, you are to use `fold-expr` to write some functions that fold expressions in some specific ways.

Function `expr-to-number` takes `expr` as input, evaluates it as an arithmetic expression, and returns the resulting number.
Complete the implementation of `expr-to-number` below by **only** giving the appropriate inputs to `fold-expr`.
In other words, you are only allowed to replace "Complete me"s with your answers but not modify the given skeleton of `expr-to-number`.
We will check this when grading.
|#

(module+ test
  (check-equal? (expr-to-number 3) 3)
  (check-equal? (expr-to-number (Neg 3)) -3)
  (check-equal? (expr-to-number (Plus 2 3)) 5)
  (check-equal? (expr-to-number (Times 2 3)) 6)
  (check-equal? (expr-to-number (Plus 4 (Times 2 3))) 10)
  (check-equal? (expr-to-number (Times 2 (Plus (Neg 1) 4))) 6))

(define (expr-to-number expr)
  (fold-expr identity
             -
             + 
             *
             expr))

#|
The function `expr-to-list` takes `expr` as input and converts it to its list representation (or a number if `expr` is just a number). 
See the tests for the expected format of the outputs.
Complete the implementation of `expr-to-list` below by **only** giving the appropriate inputs to `fold-expr`.
Again, you are only allowed to replace "Complete me"s with your answers but not modify the given skeleton of `expr-to-list`.
We will check this when grading.
|#

(module+ test
  (check-equal? (expr-to-list 3) 3)
  (check-equal? (expr-to-list (Neg 3)) '(- 3))
  (check-equal? (expr-to-list (Plus 2 3)) '(2 + 3))
  (check-equal? (expr-to-list (Times 2 3)) '(2 * 3))
  (check-equal? (expr-to-list (Plus 4 (Times 2 3))) '(4 + (2 * 3)))
  (check-equal? (expr-to-list (Times 2 (Plus (Neg 1) 4))) '(2 * ((- 1) + 4))))

(define (expr-to-list expr)
  (fold-expr identity
             (lambda (x) `(- ,x)) 
             (lambda (l r) `(,l + ,r)) 
             (lambda (l r) `(,l * ,r)) 
             expr))
