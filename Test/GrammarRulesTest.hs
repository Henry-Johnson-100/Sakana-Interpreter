import Test.Tasty
import Test.Tasty.HUnit
import Data.List

import Grammar.GrammarRules

-- | main
main = do
    defaultMain tests

-- | All GR tests
tests = testGroup "Token.Util.EagerCollapsible Tests" testList where
    testList =
        [

        ]
