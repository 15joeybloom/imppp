module Test.IO where

import           Test.Tasty
import           Test.Tasty.HUnit

import Data.Map.Strict as M

import           IO

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [ testCase "sum" $ do
        let (Right ImpState { store }, _) = runPgm sumPgm []
        M.lookup "n" store @?= Just 0
        M.lookup "sum" store @?= Just 5050
    , testCase "uninitialized access" $ do
        let
            (Left ImpError { state = ImpState { store }, message }, _) =
                runPgm uninitializedAccessPgm []
        M.lookup "x" store @?= Just 0
        M.lookup "xx" store @?= Nothing
        message @?= "xx uninitialized!"
    , testCase "uninitialized assignment" $ do
        let
            (Left ImpError { state = ImpState { store }, message }, _) =
                runPgm uninitializedAssignPgm []
        M.lookup "x" store @?= Just 1
        M.lookup "xx" store @?= Nothing
        message @?= "xx uninitialized!"
    , testCase "division by zero" $ do
        let
            (Left ImpError { state = ImpState { store }, message }, _) =
                runPgm divZeroPgm []
        M.lookup "x" store @?= Just 0
        message @?= "Division by zero!"
    , testCase "io" $ do
        let (Right ImpState { input }, output) = runPgm ioPgm [1, 2, 3]
        input @?= [3]
        output @?= "Input two numbers: Their sum is: 3\n"
    , testCase "read EOF" $ do
        let (Left ImpError { message }, _) = runPgm readPgm []
        message @?= "Not enough input!"
    ]

sumPgm :: Pgm
sumPgm = Pgm ["n", "sum"] $ Stmts
    [ Assign "n" (I 100)
    , Assign "sum" (I 0)
    , While (Not (Leq (Var "n") (I 0))) $ Stmts
        [ Assign "sum" (Plus (Var "sum") (Var "n"))
        , Assign "n" (Plus (Var "n") (I (-1)))
        ]
    ]

uninitializedAccessPgm :: Pgm
uninitializedAccessPgm = Pgm ["x"] $ Assign "x" (Var "xx")

uninitializedAssignPgm :: Pgm
uninitializedAssignPgm = Pgm ["x"] $ Stmts
    [ Assign "x" (I 1)
    , Assign "xx" (I 2)
    ]

divZeroPgm :: Pgm
divZeroPgm = Pgm ["x"] $ Stmts
    [ Assign "x" (Div (Var "x") (I 0))
    , Assign "x" (I 1)
    ]

ioPgm :: Pgm
ioPgm = Pgm ["x", "y"] $ Stmts
    [ Print [Printable "Input two numbers: "]
    , Assign "x" Read
    , Assign "y" Read
    , Print
        [ Printable "Their sum is: "
        , Printable (Plus (Var "x") (Var "y"))
        , Printable "\n"
        ]
    ]

readPgm :: Pgm
readPgm = Pgm ["x"] $ Assign "x" Read
