#lang racket

#|

CSC324 — 2024F — Assignment 1

Intruoduction to Racket, Recursion, Pattern Matching

Tasks

1. Small racket functions: 
  (a) celsius-to-fahrenheit
  (b) remove-second
2. Recursive functions: 
  (a) collatz
  (b) better-fibonacci
    - Implement fibonacci-helper.
    - Answer 2 short answer questions.
  (c) factorial-tail
3. Pattern Matching:
  (a) area-or-volume
  (b) subst [RACKET ONLY]

All function bodies to implement are marked with "Complete me".

Recall that #; means "expression comment". Uncomment the tests as you complete the tasks. 

Note that by default, DrRacket only displays output for failing test cases. 
If you don't see any output, that means you've passed the tests. 

IMPORTANT: You are NOT allowed to modify existing imports or add new imports.

|#

#| 
We put our tests inside a test "sub-module" named `test` so DrRacket runs them at the end — this 
allows us to write tests above the functions tested.
|#
(module+ test (require rackunit))
(require racket/trace)
(provide celsius-to-fahrenheit remove-second collatz fibonacci-saq fibonacci-helper better-fibonacci-saq 
         factorial-tail area-or-volume subst)

#|
-------------------------------------------------------------------------------
 * Task 1: Small racket functions *
-------------------------------------------------------------------------------

(a) celsius-to-fahrenheit

`celsius-to-fahrenheit` takes a temperature in degrees Celsius and returns the equivalent 
temperature in degrees fahrenheit, rounded to the nearest integer.
Relevant documentation: https://docs.racket-lang.org/reference/generic-numbers.html.
Use `round` for rounding to the nearest integer.
|#

(module+ test
  (check-equal? (celsius-to-fahrenheit 0) 32)
  (check-equal? (celsius-to-fahrenheit 8) 46)
  (check-equal? (celsius-to-fahrenheit -20) -4)
  )

(define (celsius-to-fahrenheit temp)
  (round(+ (/(* temp 9) 5) 32)))

#|
(b) remove-second

`remove-second` takes a list as input, removes the second element of the list.
If the inputted list has less than 2 elements, `remove-second` returns the list without modifying it.
Relevant documentation: https://docs.racket-lang.org/reference/pairs.html
|#

(module+ test
  (check-equal? (remove-second '(2 1 3 1)) '(2 3 1))
  (check-equal? (remove-second '(a b c)) '(a c))
  (check-equal? (remove-second '((a b c) (1 2 3) (A B C))) '((a b c) (A B C)))
  (check-equal? (remove-second '(first second)) '(first))
  (check-equal? (remove-second '(only)) '(only))
  )

(define (remove-second xs)
  (if (< (length xs) 2)
      xs
      (cons (car xs) (list-tail xs 2))
      ))

#|
-------------------------------------------------------------------------------
 * Task 2: Recursive functions *
-------------------------------------------------------------------------------

(a) collatz

`collatz` takes a positive integer `n` and returns the collatz conjecture sequence as described in 
the handout, starting at `n` and ending at 1. (The sequence starting at `n` is guaranteed to reach 1).
|#

#;(module+ test
    (check-equal? (collatz 5) '(5 16 8 4 2 1))
    (check-equal? (collatz 12) '(12 6 3 10 5 16 8 4 2 1))
    (check-equal? (collatz 8) '(8 4 2 1))
    (check-equal? (collatz 1) '(1))
    )

(define (collatz n)
  (cond
    [(equal? n 1) (cons 1 `())]
    [(even? n) (cons n (collatz (/ n 2)))]
    [else (cons n (collatz (+ (* n 3) 1)))]
    ))

#|
(b) better-fibonacci

`fibonacci` takes a non-negative integer `n` and returns the n-th element of the fibonacci sequence. 
See below for a simple implementation of `fibonacci`.
|#

(define (fibonacci n)
  (if (< n 2) 1
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

#|
--- SHORT ANSWER QUESTION ---
QUESTION 1: When calling (fibonacci 5), how many time is `fibonacci` called (including the initial 
call and all recursive calls)? 

Instructions: Assign your answer as a number (NOT a string) to the variable `better-fibonacci-saq` 
below by replacing the "Complete me" with your answer.
|#

(define fibonacci-saq 15)

#|
Hint: To check your answer, you can uncomment the two following lines:
|#
#;(trace fibonacci)
#;(fibonacci 5)
#|
Then you can count the number of calls to `fibonacci` in the trace.
IMPORTANT: If you do this, re-comment the two lines of code before submitting.
|#

#|
Example: When calling (fibonacci 2), `fibonacci` is called 3 times:
  1) Our direct call (fibonacci 2).
  2) Recursive call (fibonacci 1).
  3) Recursive call (fibonacci 0).
Note that (fibonacci 1) and (fibonacci 0) don't make any recursive calls.
Then, in this example, you would answer:
  (define fibonacci-saq 3)
-----------------------------
|#

#|
Now, we will implement `fibonacci` more efficiently. `better-fibonacci` behaves the same as `fibonacci`.
In particular, `better-fibonacci` takes a non-negative integer `n` and returns the n-th element of 
the fibonacci sequence. 

`better-fibonacci` uses a helper function called `fibonacci-helper`, which takes an integer `n` and 
returns a pair of the (n-1)-th and n-th elements of the fibonacci sequence.
Note that (fibonacci-helper 0) returns (cons 0 1) and (fibonacci-helper 1) returns (cons 1 1) directly.

Hint: `let` might be useful: https://docs.racket-lang.org/reference/let.html
|#

(module+ test
  (check-equal? (fibonacci-helper 0) (cons 0 1))
  (check-equal? (fibonacci-helper 1) (cons 1 1))
  (check-equal? (fibonacci-helper 2) (cons 1 2))
  (check-equal? (fibonacci-helper 4) (cons 3 5))
  (check-equal? (fibonacci-helper 6) (cons 8 13))
  )

(define (fibonacci-helper n)
  (cond
    [(= n 0) (cons 0 1)]
    [(= n 1) (cons 1 1)]
    [else (let(
               [e1 (fibonacci-helper(- n 1))])
            (cons (cdr e1) (+ (car e1) (cdr e1))))
          ]))


#|
`better-fibonacci` is implemented for you below. Note that if `fibonacci-helper` is implemented 
correctly, this simple implementation of `better-fibonacci` works.
|#

#;(module+ test
    (check-equal? (better-fibonacci 0) 1)
    (check-equal? (better-fibonacci 2) 2)
    (check-equal? (better-fibonacci 4) 5)
    (check-equal? (better-fibonacci 6) 13)
    )

(define (better-fibonacci n)  
  (cdr (fibonacci-helper n)))

#|
Note that `fibonacci-helper` could be implemented as a nested function:
  (define (better-fibonacci n)  
    (define (fibonacci-helper n)
        ...)
    (cdr (fibonacci-helper n)))
Here we have implemented it as a separate function so we can test it separately.
|#

#|
--- SHORT ANSWER QUESTION ---
QUESTION 2: When calling (better-fibonacci 5), how many time is `fibonacci-helper` called? 

Instructions: Assign your answer as a number (NOT a string) to the variable `fibonacci-saq` 
below by replacing the "Complete me" with your answer.
|#

(define better-fibonacci-saq 5)

#|
Hint: To check your answer, you can uncomment the two following lines:
|#
#;(trace fibonacci-helper)
#;(better-fibonacci 5)
#|
Then you can count the number of calls to `fibonacci-helper` in the trace.
IMPORTANT: If you do this, re-comment the two lines of code before submitting.
|#

#|
Example: When calling (better-fibonacci 2), `fibonacci-helper` is called 2 times:
  1) Direct call (fibonacci 2).
  2) Recursive call (fibonacci 1).
Note that (fibonacci 1) doesn't make any recursive calls.
Then, in this example, you would answer:
  (define better-fibonacci-saq 2)
-----------------------------
|#

#|
(c) factorial-tail

Consider the following simple implementation of `factorial`. This implementation does NOT use tail recursion.
|#

(define (factorial n)
  (if (= n 1) 1
      (* n (factorial (- n 1)))))

#|
Implement `factorial-tail` using TAIL RECURSION.

Hint: You should use an "accumulator" input, which keeps track of the result "up to now". 
The starter code below includes an additional input `acc` to reflect this.
You may assume that in all tests, the value of input `acc` is 1 in the initial calls to `factorial-tail`.
(However, if your implementation is correct,
the value of `acc` will be greater than 1 in some recursive calls.)
|#

(define (factorial-tail n acc)
  (if (= n 1) acc
      (factorial-tail (- n 1) (* n acc) ))
  )

#;(module+ test
    (check-equal? (factorial-tail 1 1) 1)
    (check-equal? (factorial-tail 3 1) 6)
    (check-equal? (factorial-tail 5 1) 120)
    )

#|
-------------------------------------------------------------------------------
* Task 3: Pattern Matching *
-------------------------------------------------------------------------------

(a) area-or-volume

shape = (list 'circle <radius>)
      | (list 'triangle <base> <height>)
      | (list 'square <side>)
      | (list 'rectangle <width> <height>) 
      | (list 'sphere <radius>)
      | (list 'cube <side>)
      | (list 'prism <base> <height>) 

`area-or-volume` takes a `shape` (described as above). If `shape` is a 2d-shape, it returns its area.
If `shape` is a 3d-shape, `it returns its volume.

IMPORTANT: Assume pi = 3.
|#

#;(module+ test
    (check-equal? (area-or-volume '(circle 5)) 75)
    (check-equal? (area-or-volume '(triangle 3 4)) 6)
    (check-equal? (area-or-volume '(square 5)) 25)
    (check-equal? (area-or-volume '(rectangle 3 4)) 12)
    (check-equal? (area-or-volume '(sphere 5)) 500)
    (check-equal? (area-or-volume '(cube 5)) 125)
    (check-equal? (area-or-volume '(prism (square 5) 2)) 50)
    (check-equal? (area-or-volume '(prism (triangle 3 4) 2)) 12)
    )

(define/match (area-or-volume shape)
  [((list op e1 e2))
   (cond
     [(equal? op 'triangle) (/ (* e1 e2) 2)]
     [(equal? op 'rectangle) (* e1 e2)]
     [(equal? op 'prism) (* e2 (area-or-volume e1))]
     )
   ]
  [((list op e1))
   (cond
     [(equal? op 'circle) (* e1 e1 3)]
     [(equal? op 'square) (* e1 e1)]
     [(equal? op 'sphere) (* 4 e1 e1 e1)]
     [(equal? op 'cube) (* e1 e1 e1)]
     )
   ]  
  
  [(num) num]
  
  )

#|
(b) subst

We will consider a simple language with the following syntax:

expr = (λ (<id>) <expr>)  ["λ expr"]
     | (<expr> <expr>)  ["function call expr"]
     | (+ <expr> <expr>)  ["add expr"]
     | <id>  ["variable expr"]
     | <int-literal>  ["literal expr"]

`subst` takes as input an `expr`, an `id`, and a `val` (which is an expr itself).
`subst` substitutes "free" occurences of `id` in `expr` with `val`, but leaves "bound" occurences 
of `id` in `expr` unchanged.

You can assume that inputs will be syntactically correct.
|#

(module+ test
    (check-equal? (subst 'x 'x 3) 3)
    (check-equal? (subst 'y 'x 3) 'y)
    (check-equal? (subst 'x 'x 'y) 'y)
    (check-equal? (subst '(+ x y) 'x '(+ y y)) '(+ (+ y y) y))
    (check-equal? (subst '((λ (y) y) x) 'x 10) '((λ (y) y) 10))
    (check-equal? (subst '((λ (x) x) x) 'x 10) '((λ (x) x) 10))
    )

(define (subst expr id val)
  (match expr
    [(list fnexpr arg)
     (list (subst fnexpr id val) (subst arg id val))]
    
    [(list 'lambda (list param) body)
     (if (equal? id param)
         expr
         (list 'lambda (list param) (subst body id val)))
     ]

    [(list '+ e1 e2)
     (list '+ (subst e1 id val) (subst e2 id val))]

    [atom
     (if (equal? atom id) val atom)]
      )
  )
