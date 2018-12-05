module Test.Increment where

import           Test.Tasty
import           Test.Tasty.HUnit

import Data.Map.Strict as M

import           Increment

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [ testCase "sum" $ do
        let Right (Store store) = runPgm sumPgm
        M.lookup "n" store @?= Just 0
        M.lookup "sum" store @?= Just 5050
    , testCase "uninitialized access" $ do
        let
            Left (ImpError { store = Store store, message }) =
                runPgm uninitializedAccessPgm
        M.lookup "x" store @?= Just 0
        M.lookup "xx" store @?= Nothing
        message @?= "xx uninitialized!"
    , testCase "uninitialized assignment" $ do
        let
            Left (ImpError { store = Store store, message }) =
                runPgm uninitializedAssignPgm
        M.lookup "x" store @?= Just 1
        M.lookup "xx" store @?= Nothing
        message @?= "xx uninitialized!"
    , testCase "division by zero" $ do
        let
            Left (ImpError { store = Store store, message }) =
                runPgm divZeroPgm
        M.lookup "x" store @?= Just 0
        message @?= "Division by zero!"
    , testCase "inc" $ do
        let Right (Store store) = runPgm incPgm
        M.lookup "x" store @?= Just 3
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

incPgm :: Pgm
incPgm = Pgm ["x"] $ Stmts
    [ Assign "x" (I 0)
    , Assign "x" (Plus (Inc "x") (Inc "x"))
    ]
