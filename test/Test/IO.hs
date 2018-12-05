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
        let (ImpState { store }, _) = runPgm sumPgm []
        M.lookup "n" store @?= Just 0
        M.lookup "sum" store @?= Just 5050
    , testCase "io" $ do
        let (_, output) = runPgm ioPgm [1, 2]
        output @?= "Input two numbers: Their sum is: 3\n"
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
