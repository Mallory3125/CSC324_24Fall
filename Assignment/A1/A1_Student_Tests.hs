module A1_Student_Tests where
{-
CSC324 — 2024F — Assignment 1 Student Tests
-}

import Prelude (IO, ($))
import A1
    ( areaOrVolume,
      betterFibonacci,
      celsiusToFarenheit,
      collatz,
      factorialTail,
      fibonacciHelper,
      removeSecond,
      Shape(..) )
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain (testGroup "A1 Tests" [celsiusToFarenheitTests, removeSecondTests, collatzTests, fibonacciHelperTests,
                                          betterFibonacciTests, factorialTailTests, areaOrVolumeTests])

{-
-------------------------------------------------------------------------------
 * Task 1: Small haskell functions *
-------------------------------------------------------------------------------
-}

-- (a) celsiusToFahrenheit
celsiusToFarenheitTests = testGroup "celsiusToFahrenheit Tests"
    [
        testCase "0C"   $ celsiusToFarenheit 0     @?= 32,
        testCase "8C"   $ celsiusToFarenheit 8     @?= 46,
        testCase "-20C" $ celsiusToFarenheit (-20) @?= -4
    ]

-- (b) removeSecond
removeSecondTests = testGroup "removeSecond Tests"
    [
        testCase "ints" $       
            removeSecond [2, 1, 3, 1]                                        @?= [2, 3, 1],
        testCase "chars" $         
            removeSecond ['a', 'b', 'c']                                     @?= ['a', 'c'],
        testCase "nested lists" $
            removeSecond [['a', 'b', 'c'], ['1', '2', '3'], ['A', 'B', 'C']] @?= [['a', 'b', 'c'], ['A', 'B', 'C']],
        testCase "2 elements" $             
            removeSecond ["first", "second"]                                 @?= ["first"],
        testCase "1 element" $             
            removeSecond ["only"]                                            @?= ["only"]
    ]

{-
-------------------------------------------------------------------------------
* Task 2: Recursive functions *
-------------------------------------------------------------------------------
-}

-- (a) collatz
collatzTests = testGroup "collatz Tests"
    [
        testCase "5"   $ collatz 5  @?= [5, 16, 8, 4, 2, 1],
        testCase "12"  $ collatz 12 @?= [12, 6, 3, 10, 5, 16, 8, 4, 2, 1],
        testCase "8"   $ collatz 8  @?= [8, 4, 2, 1],
        testCase "1"   $ collatz 1  @?= [1]
    ]

-- (b) betterFibonacci
fibonacciHelperTests = testGroup "fibonacciHelper Tests"
    [
        testCase "fibonacciHelper 0"  $ fibonacciHelper 0 @?= (0, 1),
        testCase "fibonacciHelper 1"  $ fibonacciHelper 1 @?= (1, 1),
        testCase "fibonacciHelper 2"  $ fibonacciHelper 2 @?= (1, 2),
        testCase "fibonacciHelper 4"  $ fibonacciHelper 4 @?= (3, 5),
        testCase "fibonacciHelper 6"  $ fibonacciHelper 6 @?= (8, 13)
    ]

betterFibonacciTests = testGroup "betterFibonacci Tests"
    [
        testCase "betterFibonacci 0"  $ betterFibonacci 0 @?= 1,
        testCase "betterFibonacci 2"  $ betterFibonacci 2 @?= 2,
        testCase "betterFibonacci 4"  $ betterFibonacci 4 @?= 5,
        testCase "betterFibonacci 6"  $ betterFibonacci 6 @?= 13
    ]

-- (c) factorialTail
factorialTailTests = testGroup "factorialTail Tests"
    [
        testCase "factorialTail 1 output" $ factorialTail 1 1 @?= 1,
        testCase "factorialTail 3 output" $ factorialTail 3 1 @?= 6,
        testCase "factorialTail 5 output" $ factorialTail 5 1 @?= 120
    ]

{-
-------------------------------------------------------------------------------
 * Task 3: Pattern Matching *
-------------------------------------------------------------------------------
-}

-- (a) areaOrVolume
areaOrVolumeTests = testGroup "areaOrVolume Tests"
    [
        testCase "circle 5"               $ areaOrVolume (Circle 5)               @?= 75,
        testCase "triangle 3"             $ areaOrVolume (Triangle 3 4)           @?= 6,
        testCase "square 5"               $ areaOrVolume (Square 5)               @?= 25,
        testCase "rectangle 3 4"          $ areaOrVolume (Rectangle 3 4)          @?= 12,
        testCase "sphere 5"               $ areaOrVolume (Sphere 5)               @?= 500,
        testCase "cube 5"                 $ areaOrVolume (Cube 5)                 @?= 125,
        testCase "prism (square 5) 2"     $ areaOrVolume (Prism (Square 5) 2)     @?= 50,
        testCase "prism (triangle 3 4) 2" $ areaOrVolume (Prism (Triangle 3 4) 2) @?= 12

    ]