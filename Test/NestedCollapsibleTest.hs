import Test.Tasty
import Test.Tasty.HUnit
import Data.List

import Token.Util.NestedCollapsible as NC

main = do
    defaultMain tests

tests = testGroup "Token.Util.NestedCollapsible Tests" testList where
    testList =
        [
            iCNC_tests
        ]

-- | isCompleteNestedCollapsible Tests as iCNC
iCNC_tests = testGroup "Token.Util.NestedCollapsible.isCompleteNestedCollapsible Tests" testList where
    testList =
        [
            iCNC_returns_false_for_empty_list,
            iCNC_identifies_a_list_with_one_complete_unnested_NC,
            iCNC_identifies_a_list_with_one_nested_NC,
            iCNC_returns_false_for_a_list_with_mismatched_terminal_counts
        ]

iCNC_returns_false_for_empty_list = testCase name assertion where
    name      = "Empty list returns False"
    assertion = assertEqual d a f
    d         = "An empty list contains no nested collapsibles"
    a         = False
    f         = isCompleteNestedCollapsible ('(' == ) ( ')' == ) ""

iCNC_identifies_a_list_with_one_complete_unnested_NC = testCase name assertion where
    name      = "A list containing a complete NC bounded by (, ) is returns True" 
    assertion = assertEqual d a f
    d         = "def some_func(a,b):"
    a         = True
    f         = isCompleteNestedCollapsible ('(' == ) ( ')' == ) "def some_func(a,b):"

iCNC_identifies_a_list_with_one_nested_NC = testCase name assertion where
    name      = "isCompleteNestedCollapsible returns True for a list with an NC nested in another NC"
    assertion = assertEqual d a f
    d         = "NestedCollapsibles can contain nested collapsibles"
    a         = True
    f         = isCompleteNestedCollapsible ('(' == ) ( ')' == ) "def some_func(a,(dfjgj),sdjk):"

iCNC_returns_false_for_a_list_with_mismatched_terminal_counts = testCase name assertion where
    name      = "returns False for a list with mismatched number of terminal counts"
    assertion = assertEqual d a f
    d         = "def some_func(a, (bc):"
    a         = False
    f         = isCompleteNestedCollapsible ('(' == ) ( ')' == ) "def some_func(a, (bc):"