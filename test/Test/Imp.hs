module Test.Imp where

import           Test.Tasty
import           Test.Tasty.HUnit

import Data.Map.Strict as M

import           Imp

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [ testCase "sum" $ do
        let Store state = runPgm sumPgm
        M.lookup "n" state @?= Just 0
        M.lookup "sum" state @?= Just 5050
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
