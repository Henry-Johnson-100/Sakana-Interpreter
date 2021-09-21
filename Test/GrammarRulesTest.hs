import Test.Tasty
import Test.Tasty.HUnit
import Data.List

import Grammar.GrammarRules
import ParseTree

-- | main
main = do
    defaultMain tests

-- | All GR tests
tests = testGroup "Grammar.GrammarRules Tests" testList where
    testList =
        [

        ]
