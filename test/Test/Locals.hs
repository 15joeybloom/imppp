module Test.Locals where

import           Test.Tasty
import           Test.Tasty.HUnit

import Data.Map.Strict as M

import           Locals

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [ testCase "sum" $ do
        let Right (Store store) = runImp sumPgm
        M.lookup 0 store @?= Just 0
        M.lookup 1 store @?= Just 5050
    , testCase "uninitialized access" $ do
        let
            Left (ImpError { env = Env env, store = Store store, message }) =
                runImp uninitializedAccessPgm
        do { loc <- M.lookup "x" env ; M.lookup loc store } @?= Just 0
        M.lookup "xx" env @?= Nothing
        message @?= "xx uninitialized!"
    , testCase "uninitialized assignment" $ do
        let
            Left (ImpError { env = Env env, store = Store store, message }) =
                runImp uninitializedAssignPgm
        do { loc <- M.lookup "x" env ; M.lookup loc store } @?= Just 1
        M.lookup "xx" env @?= Nothing
        message @?= "xx uninitialized!"
    , testCase "division by zero" $ do
        let
            Left (ImpError { env = Env env, store = Store store, message }) =
                runImp divZeroPgm
        do { loc <- M.lookup "x" env ; M.lookup loc store } @?= Just 0
        message @?= "Division by zero!"
    , testCase "locals" $ do
        let Right (Store store) = runImp localsPgm
        M.lookup 0 store @?= Just 4
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
    [ Declare ["x", "y"]
    , Assign "y" (I 1)
    , xPlusEqualsY
    , Stmts
        [ Declare ["y"]
        , Assign "y" (I 2)
        , xPlusEqualsY
        ]
    , xPlusEqualsY
    ]
  where
    xPlusEqualsY = Assign "x" (Plus (Var "x") (Var "y"))
