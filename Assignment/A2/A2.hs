module A2 where

{-
CSC324 — 2024F — Assignment 2

Higher-Order Functions, Currying, and Evaluation

Tasks

1. Higher Order Functions [Racket only; See Racket file for Task 1.]
2. Currying
  (a) curry, uncurry, curry3, and uncurry3
  (b) zip, unzip, and zipWith
3. Evaluation

Replace all "func = undefined" with a valid definition (or definitions) for "func".

IMPORTANT: You are NOT allowed to modify existing imports or add new imports.
-}

{-
CSC324 — 2024F — Assignment 2

Higher-Order Functions, Currying, and Evaluation

Tasks

1. Higher Order Functions [Racket only; See Racket file for Task 1.]
2. Currying
  (a) curry, uncurry, curry3, and uncurry3
  (b) zip, unzip, and zipWith
3. Evaluation

Replace all "func = undefined" with a valid definition (or definitions) for "func".

IMPORTANT: You are NOT allowed to modify existing imports or add new imports.
-}
import Prelude
  ( Eq, Int, Maybe (..), Show, String, map, max, maximum, min, minimum, otherwise, sum, undefined, ($), (*), (+), (.), (==), Bool (..), (&&), break)
import Test.Tasty.Patterns.Types ()

{-
-------------------------------------------------------------------------------
* Task 2: Currying *
-------------------------------------------------------------------------------

(a) curry, uncurry, curry3, and uncurry3

Implement functions `curry` and `uncurry` that convert between the two representations of functions with two inputs.

`curry` takes as input an uncurried function and returns an equivalent curried function.

`uncurry` takes as input a curried function and returns an equivalent uncurried function.
-}

curry :: ((a, b) -> c) -> (a -> b -> c)
curry f =  \g -> \x -> f (g,x)


uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f  = \(g,x) -> f g x

{-
Now implement curry3 and uncurry3 similarly, but for functions with 3 inputs instead of 2.
-}

curry3 :: ((a, b, c) -> d) -> (a -> b -> c -> d)
curry3 f = \g -> \x -> \y -> f (g,x,y)

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f = \(g,x,y) -> f g x y

{-
(b) zip, unzip, and zipWith

Implement `zip` that takes two lists `xs` and `ys` and returns a list of pairs `zs`.
If `x` and `y` are the k-th elements of `xs` and `ys` respectively, the k-th element of `zs` is a pair `(x, y)`.  If one input list is longer than the other, then the remaining elements in the longer list are ignored.
For example:
  zip [1, 2, 3] ['a', 'b'] = [(1, 'a'), (2, 'b')]
-}

zip :: [a] -> [b] -> [(a, b)]
zip (x:xs) (y:ys) = (x,y) : zip xs ys
zip _ _ = []


{-
Implement `unzip` that takes a list of pairs `zs` and returns a pair of lists `(xs, ys)`. If `(x, y)` is the k-th
element of the input list, then `x` and `y` are the k-th elements of `xs` and `ys` respectively.
Moreover, `xs` and `ys` have the same length as the input list.
For example:
  unzip [(1, 'a'), (2, 'b'), (3, 'c')] == ([1, 2, 3], ['a', 'b', 'c'])
-}

unzip :: [(a, b)] -> ([a], [b])
unzip [] = ([], [])
unzip ((a,b): zs) = (a: as, b : bs) where (as,bs) = unzip zs

{-
Implement `zipWith` that takes two lists `xs` and `ys`, and a function `f` which can combine elements
of `xs` and `ys`, and returns a list `zs`. If x and y are the k-th elements of `xs` and `ys`
respectively, the k-th element of `zs` is the result of calling `f x y`.
For example:
  zipWith (+) [1, 2, 3] [2, 4, 6] == [3, 6, 9]
-}

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys
zipWith f _ _ = []


{-
-------------------------------------------------------------------------------
* Task 3: Evaluation *
-------------------------------------------------------------------------------

Note: You may wish to finish Week 5's lecture to know more about how to work with
algebraic data types before attempting this task.

The language from A1 can be defined as the following algebraic data type in Haskell
(you can ignore the deriving clause for this task):
-}

data Expr
  = Lam String Expr
  | App Expr Expr
  | Add Expr Expr
  | Id String
  | Lit Int
  deriving (Show, Eq)

{-
Recall from A1: An occurrence of an identifier `id` is "bound" if it is inside some enclosing λ term
whose parameter is `id`. An occurrence of an identifier is "free" if it is not bound.

(a) `evalEagerSubst`

Recall the function `subst` which takes as input an `expr`, an `id`, and a `val` (which is an expression itself),
substitutes free occurrences of `id` in `expr` with `val`, and leaves bound occurrences of `id` in `expr` unchanged.

You are given an implementation of `subst` below.
You will need to use `subst` as a helper function in this subtask.
-}

subst :: Expr -> String -> Expr -> Expr
subst (Add expr1 expr2) id val = Add (subst expr1 id val) (subst expr2 id val)
subst (App func arg) id val = App (subst func id val) (subst arg id val)
subst (Lam param body) id val
  | param == id = Lam param body
  | otherwise = Lam param (subst body id val)
subst (Id oldId) id val
  | oldId == id = val
  | otherwise = Id oldId
subst (Lit n) _ _ = Lit n

{-
Function `evalEagerSubst` takes as input an expression of type `Expr`,
evaluates it eagerly (as described below), 
and returns a `Maybe Expr` value as the result.
When the evaluation succeeded, the result is an expression wrapped by `Just`;
otherwise, the result is `Nothing`.

In this task, you need to handle invalid reduction cases (e.g. adding two non-integer expressions).
For such cases, return `Nothing`.

Hint: Detailed semantics of `evalEagerSubst`:
  - The evaluation of literal or λ-term is itself wrapped by `Just`.
  - The evaluation of an add expr is the sum of the eagerly evaluated values of its arguments. 
    More precisely, to evaluate `(Add expr1 expr2)`:
      1. Evaluate each of `expr1` and `expr2` separately.
      2. If both results are integer literals wrapped by `Just`, add these results
         and return the resulting integer literal wrapped by `Just`;
         otherwise, return `Nothing`.
  - To evaluate a function call, first evaluate the function, then the argument,
    and finally the function body after substituting the evaluated argument into the function body.
    More precisely, to evaluate `(App func arg)`:
      1. Evaluate `func`. 
         The result should be a λ term `(Lam id body)` wrapped by `Just`;
         otherwise, return `Nothing`.
      2. Evaluate `arg`.
         The result should be a `Just` value;
         otherwise, return `Nothing`.
      3. Substitute free occurrences of variable term `id` in `body` with the evaluated argument.
         You may use `subst` as a helper for this step.
      4. Evaluate the function body after substitution.
  - Return `Nothing` for free identifiers.
-}

evalEagerSubst :: Expr -> Maybe Expr
evalEagerSubst (Id a) = Nothing
evalEagerSubst (Lit a) = Just (Lit a)
evalEagerSubst (Lam a b) = Just (Lam a b)
evalEagerSubst (Add expr1 expr2) = do
  val1 <- evalEagerSubst expr1  
  val2 <- evalEagerSubst expr2  
  case (val1, val2) of
    ( Lit a,  Lit b) -> Just (Lit (a + b)) 
    _                -> Nothing 
evalEagerSubst (App func arg) = do
  val1 <- evalEagerSubst func  
  val2 <- evalEagerSubst arg  
  case (val1, val2) of
    ( Lam id body, val ) -> evalEagerSubst ( subst body id val) 
    _                -> Nothing 


{-

(b) evalEagerEnv

Recall how we used Closure and Environments instead of substitution
to implement an interpreter during the lecture.
Now you are required to implement it in Haskell.

An environment is a mapping of name-value bindings.
You are given a simple implementation of a `Map` data structure below.
-}

data Map k v = Empty | Entry k v (Map k v) deriving (Show)

hasProj :: (Eq k, Eq v) => Map k v -> Map k v -> Bool
hasProj Empty _ = True
hasProj (Entry k v m) m' = case lookup k m' of
  Just v' -> v == v' && hasProj m m'
  _ -> False

instance (Eq k, Eq v) => Eq (Map k v) where
  m1 == m2 = hasProj m1 m2 && hasProj m2 m1

lookup :: Eq k => k -> Map k v -> Maybe v
lookup k Empty = Nothing
lookup k (Entry k' v m) = if k == k' then Just v else lookup k m

insert :: Eq k => k -> v -> Map k v -> Map k v
insert k v Empty = Entry k v Empty
insert k v (Entry k' v' m) = if k == k' then Entry k v m else Entry k' v' (insert k v m)

type Env = Map String Value -- An environment is a mapping of names to values

{-
A value is either a number or a closure, defined as the following algebraic data type.
-}

data Value
  = Num Int
  | Closure String Expr Env
  deriving (Show, Eq)

{-
A number is a value, constructed by the integer value;
A closure is a function with the environment in which the function was evaluated.
A closure value is constructed by the parameter identifier, the function body (an expression),
and its environment.

Function `evalEagerEnv` takes as input an expression of type `Expr` and an environment `env`,
evaluates the expression with the given environment eagerly,
and returns a `Maybe Value` value as the result.
When the evaluation succeeded, the result is a value wrapped by `Just`; otherwise, the result is `Nothing`.

In this task, you need to handle invalid reduction cases (e.g. adding two non-integer expressions).
For such cases, return `Nothing`.

Hint: Detailed semantics of `evalEagerEnv` (note the differences between the previous subtask):
  - The evaluation of literal is a number value wrapped by `Just`.
  - The evaluation of a λ term is a closure capturing the current environment and wrapped by `Just`.
  - The evaluation of an add expr is the sum of the eagerly evaluated values of its arguments. 
    More precisely, to evaluate `(Add expr1 expr2)`:
      1. Evaluate each of `expr1` and `expr2` separately.
      2. If both results are integer values wrapped by `Just`, add these results
         and return the resulting integer value wrapped by `Just`;
         otherwise, return `Nothing`.
  - To evaluate a function call, first evaluate the function to a closure, then the argument,
    and finally the function body under the closure's context extended with a binding of the argument value.
    More precisely, to evaluate `(App func arg)`:
      1. Evaluate `func`. 
         The result should be a closure `(Closure id body funcEnv)` wrapped by `Just`;
         otherwise, return `Nothing`.
      2. Evaluate `arg`.
         The result should be a `Just` value;
         otherwise, return `Nothing`.
      3. Insert the binding from `id` to the value that `arg` was evaluated to
         to the closure's environment `funcEnv`, as a new environment `newEnv`.
      4. Evaluate the function body under the new environment `newEnv`.
  - To evaluate an identifier, look the name up in the current environment.
    Return `Nothing` for free identifiers that are unbound in the environment.
    (`lookup` already does so.)

-}

evalEagerEnv :: Expr -> Env -> Maybe Value
evalEagerEnv (Lit a) env = Just (Num a)
evalEagerEnv (Id a) env = lookup a env
evalEagerEnv (Lam str expr) env = Just (Closure str expr env)
evalEagerEnv (Add expr1 expr2) env = do
    val1 <- evalEagerEnv expr1 env 
    val2 <- evalEagerEnv expr2 env
    case (val1, val2) of
        (Num a, Num b) -> Just (Num (a + b))
        _ -> Nothing    
evalEagerEnv (App func arg) env = do
    val1 <- evalEagerEnv func env
    val2 <- evalEagerEnv arg env
    case (val1, val2) of
        (Closure id body funcEnv, val) -> evalEagerEnv body newEnv where newEnv = insert id val funcEnv
        _ -> Nothing
