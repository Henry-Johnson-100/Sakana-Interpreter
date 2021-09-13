import Test.Tasty
import Test.Tasty.HUnit
import Data.List

import Token.Util.NestedCollapsible as NC

main = do
    defaultMain tests

tests = testGroup "Token.Util.NestedCollapsible Tests" testList where
    testList =
        [
            iNC_tests
        ]

-- | isNestedCollapsible Tests as iNC
iNC_tests = testGroup "Token.Util.NestedCollapsible.isNestedCollapsible Tests" testList where
    testList =
        [
            iNC_returns_false_for_empty_list,
            iNC_identifies_a_list_with_one_complete_unnested_NC
        ]

iNC_returns_false_for_empty_list = testCase name assertion where
    name      = "Empty list returns False"
    assertion = assertEqual d a f
    d         = "An empty list contains no nested collapsibles"
    a         = False
    f         = isNestedCollapsible ('(' == ) ( ')' == )

iNC_identifies_a_list_with_one_complete_unnested_NC = testCase name assertion where
    name      = "A list containing a complete NC bounded by (, ) is returns True" 
    assertion = assertEqual d a f
    d         = "def some_func(a,b):"
    a         = True
    f         = isNestedCollapsible ('(' == ) ( ')' == ) "def some_func(a,b):"