import Test.Tasty
import Test.Tasty.HUnit
import Data.List

import Lexer

-- | main
main = do
    defaultMain tests

-- | All EC tests
tests = testGroup "Lexer Tests" testList where
    testList =
        [

        ]