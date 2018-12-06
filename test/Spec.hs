import           Test.Tasty

import qualified Test.Imp as Imp
import qualified Test.Increment as Increment
import qualified Test.IO as IO
import qualified Test.Locals as Locals
import qualified Test.Halt as Halt
import qualified Test.Imppp as Imppp

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ testGroup "Imp tests" [Imp.tests]
    , testGroup "Increment tests" [Increment.tests]
    , testGroup "IO tests" [IO.tests]
    , testGroup "Locals tests" [Locals.tests]
    , testGroup "Halt tests" [Halt.tests]
    , testGroup "Imppp tests" [Imppp.tests]
    ]
