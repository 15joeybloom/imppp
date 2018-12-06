module Test.Imppp where

import           Test.Tasty
import           Test.Tasty.HUnit

import Data.Map.Strict as M

import           Imppp

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [ testCase "sum" $ do
        let (Nothing, output) = runImp sumPgm []
        output @?= "5050"
    , testCase "uninitialized access" $ do
        let (Just e, _) = runImp uninitializedAccessPgm []
            ImpError { env = Env env, state, message } = e
            ImpState { store } = state
        do { loc <- M.lookup "x" env ; M.lookup loc store } @?= Just 0
        M.lookup "xx" env @?= Nothing
        message @?= "xx uninitialized!"
    , testCase "uninitialized assignment" $ do
        let (Just e, _) = runImp uninitializedAssignPgm []
            ImpError { env = Env env, state, message } = e
            ImpState { store } = state
        do { loc <- M.lookup "x" env ; M.lookup loc store } @?= Just 1
        M.lookup "xx" env @?= Nothing
        message @?= "xx uninitialized!"
    , testCase "division by zero" $ do
        let (Just e, _) = runImp divZeroPgm []
            ImpError { env = Env env, state, message } = e
            ImpState { store } = state
        do { loc <- M.lookup "x" env ; M.lookup loc store } @?= Just 0
        message @?= "Division by zero!"
    , testCase "locals" $ do
        let (Nothing, output) = runImp localsPgm []
        output @?= unlines ["1", "2", "3", "2", "1"]
    , testCase "io" $ do
        let (Nothing, output) = runImp ioPgm [1, 2, 60]
        output @?= "Input two numbers: Their sum is: 3\n"
    , testCase "read EOF" $ do
        let (Just ImpError { message }, _) = runImp readPgm []
        message @?= "Not enough input!"
    , testCase "halt" $ do
        let (Nothing, output) = runImp haltPgm []
        output @?= ""
    , testCase "inc" $ do
        let (Nothing, output) = runImp incPgm []
        output @?= "3"
    ]


sumPgm :: Stmt
sumPgm = Stmts
    [ Declare ["n", "sum"]
    , Assign "n" (I 100)
    , Assign "sum" (I 0)
    , While (Not (Leq (Var "n") (I 0))) $ Stmts
        [ Assign "sum" (Plus (Var "sum") (Var "n"))
        , Assign "n" (Plus (Var "n") (I (-1)))
        ]
    , Print [Printable (Var "sum")]
    ]

uninitializedAccessPgm :: Stmt
uninitializedAccessPgm = Stmts [ Declare ["x"], Assign "x" (Var "xx") ]

uninitializedAssignPgm :: Stmt
uninitializedAssignPgm = Stmts
    [ Declare ["x"]
    , Assign "x" (I 1)
    , Assign "xx" (I 2)
    ]

divZeroPgm :: Stmt
divZeroPgm = Stmts
    [ Declare ["x"]
    , Assign "x" (Div (Var "x") (I 0))
    , Assign "x" (I 1)
    ]

localsPgm :: Stmt
localsPgm = Stmts
    [ Declare ["x"]
    , Assign "x" (I 1)
    , Print [Printable (Var "x"), Printable "\n"]
    , Stmts
        [ Declare ["x"]
        , Assign "x" (I 2)
        , Print [Printable (Var "x"), Printable "\n"]
        , Stmts
            [ Declare ["x"]
            , Assign "x" (I 3)
            , Print [Printable (Var "x"), Printable "\n"]
            ]
        , Print [Printable (Var "x"), Printable "\n"]
        ]
    , Print [Printable (Var "x"), Printable "\n"]
    ]

ioPgm :: Stmt
ioPgm = Stmts
    [ Declare ["x", "y"]
    , Print [Printable "Input two numbers: "]
    , Assign "x" Read
    , Assign "y" Read
    , Print
        [ Printable "Their sum is: "
        , Printable (Plus (Var "x") (Var "y"))
        , Printable "\n"
        ]
    ]

readPgm :: Stmt
readPgm = Stmts [ Declare ["x"], Assign "x" Read ]

haltPgm :: Stmt
haltPgm = Stmts
    [ Halt
    , Print [Printable "Should not be printed."]
    ]

incPgm :: Stmt
incPgm = Stmts
    [ Declare ["x"]
    , Assign "x" (I 0)
    , Assign "x" (Plus (Inc "x") (Inc "x"))
    , Print [Printable (Var "x")]
    ]
