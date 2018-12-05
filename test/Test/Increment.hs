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
        let Store state = runPgm sumPgm
        M.lookup "n" state @?= Just 0
        M.lookup "sum" state @?= Just 5050
    , testCase "inc" $ do
        let Store state = runPgm incPgm
        M.lookup "x" state @?= Just 3
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

incPgm :: Pgm
incPgm = Pgm ["x"] $ Stmts
    [ Assign "x" (I 0)
    , Assign "x" (Plus (Inc "x") (Inc "x"))
    ]
