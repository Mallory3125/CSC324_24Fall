module A2_Student_Tests where

{-
CSC324 — 2024F — Assignment 2 Student Tests
-}

import Prelude (IO, Num, ($), (+), (-), (++), Maybe (..), Eq)
import A2
    ( curry,
      uncurry,
      curry3,
      uncurry3,
      zip,
      unzip,
      zipWith,
      evalEagerSubst,
      Expr (..), evalEagerEnv, Value (..), Map (..), insert)
import Data.Tuple (swap)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain (testGroup "A2 Tests" [curryUncurryTests, zipUnzipTests, evalEagerSubstTests, evalEagerEnvTests])

{-
-------------------------------------------------------------------------------
* Task 2: Currying *
-------------------------------------------------------------------------------
-}

-- (a) curry, uncurry, curry3, and uncurry3
swap3 :: (a, b, c) -> (b, c, a)
swap3 (x, y, z) = (y, z, x)

addSub :: Num a => a -> a -> a -> a
addSub x y z = x + y - z

curryUncurryTests = testGroup "curry, uncurry, curry3 & uncurry3 Tests"
    [
        testCase "curry"    $ curry swap 1 2            @?= (2, 1),
        testCase "uncurry"  $ uncurry (+) (1, 2)        @?= 3,
        testCase "curry3"   $ curry3 swap3 1 2 3        @?= (2, 3, 1),
        testCase "uncurry3" $ uncurry3 addSub (2, 3, 4) @?= 1
    ]

-- (b) zip, unzip, zipWith
zipUnzipTests = testGroup "zip, unzip & zipWith Tests"
    [
        testCase "zip" $ 
            zip [1, 2, 3] ['a', 'b']                                  @?= [(1, 'a'), (2, 'b')],
        testCase "unzip" $ 
            unzip [(1, 'a'), (2, 'b'), (3, 'c')]                      @?= ([1, 2, 3], ['a', 'b', 'c']),
        testCase "zipWith Int" $ 
            zipWith (+) [1, 2, 3] [4, 6, 8]                           @?= [5, 8, 11],
        testCase "zipWith String" $ 
            zipWith (++) ["Hello ", "World "] ["Computer", "Science"] @?= ["Hello Computer", "World Science"]
    ]

{-
-------------------------------------------------------------------------------
* Task 3: Evaluation *
-------------------------------------------------------------------------------
-}

-- (b) evalEagerSubst
evalEagerSubstTests = testGroup "evalEagerSubst Tests"
    [
        testCase "add" $ 
            evalEagerSubst (Add (Lit 0) (Lit 1)) @?= 
                Just (Lit 1),
        testCase "func call" $ 
            evalEagerSubst (App (Lam "x" (Add (Id "x") (Id "x"))) (Lit 1)) @?=
                Just (Lit 2),
        testCase "higher order func call" $ 
            evalEagerSubst (App (Lam "f" (App (Id "f") (Lit 3))) (Lam "x" (Add (Lit 4) (Id "x")))) @?= 
                Just (Lit 7)
    ]

-- (b) evalEagerEnv
empty = Empty

-- construct a map from list
fromList :: (Eq k) => [(k, v)] -> Map k v
fromList [] = Empty
fromList ((k, v) : xs) = insert k v (fromList xs)

evalEagerEnvTests = testGroup "evalEagerEnv Tests"
    [
        testCase "add" $
            evalEagerEnv (Add (Lit 0) (Lit 1)) empty @?=
                Just (Num 1),
        testCase "func call" $
            evalEagerEnv (App (Lam "x" (Add (Id "x") (Id "x"))) (Lit 1)) empty @?=
                Just (Num 2),
        testCase "higher order func call" $
            evalEagerEnv (App (Lam "f" (App (Id "f") (Lit 3))) (Lam "x" (Add (Lit 4) (Id "x")))) empty @?=
                Just (Num 7),
        testCase "id with non-empty input env" $
            evalEagerEnv (Id "x") (fromList [("x", Num 42)]) @?=
                Just (Num 42)
    ]