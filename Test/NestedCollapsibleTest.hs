import Test.Tasty
import Test.Tasty.HUnit
import Data.List

import Token.Util.NestedCollapsible as NC

main = do
    defaultMain tests

tests = testGroup "Token.Util.NestedCollapsible Tests" testList where
    testList =
        [
            iCNC_tests,
            hNC_tests,
            tN_tests,
            tSD_tests
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
    name      = "A list consisting wholly of a single complete NC returns true" 
    assertion = assertEqual d a f
    d         = "(a,b)"
    a         = True
    f         = isCompleteNestedCollapsible ('(' == ) ( ')' == ) "(a,b)"

iCNC_identifies_a_list_with_one_nested_NC = testCase name assertion where
    name      = "isCompleteNestedCollapsible returns True for a list with an NC nested in another NC"
    assertion = assertEqual d a f
    d         = "NestedCollapsibles can contain nested collapsibles"
    a         = True
    f         = isCompleteNestedCollapsible ('(' == ) ( ')' == ) "(a,(dfjgj),sdjk)"

iCNC_returns_false_for_a_list_with_mismatched_terminal_counts = testCase name assertion where
    name      = "returns False for a list with mismatched number of terminal counts"
    assertion = assertEqual d a f
    d         = "def some_func(a, (bc):"
    a         = False
    f         = isCompleteNestedCollapsible ('(' == ) ( ')' == ) "(a, (bc)"

-- | hasNestedCollapsible Tests as hNC
hNC_tests = testGroup "Token.Util.NestedCollapsible.hasNestedCollapsible Tests" testList where
    testList =
        [
            hNC_returns_true_for_list_with_one_complete_and_one_incomplete_NC
        ]

hNC_returns_true_for_list_with_one_complete_and_one_incomplete_NC = testCase name assertion where
    name      = "A list with one complete and one incomplete NC returns True"
    assertion = assertEqual d a f
    d         = "hasNestedCollapsible is a more general function, that doesn't tell you if the first encountered NC is complete, just if there is a complete NC at some point."
    a         = True        
    f         = hasNestedCollapsible ('(' == ) ( ')' == ) "def some_func(a, (bc):"

-- | takeNest tests as tN
tN_tests = testGroup "takeNest Tests" testList where
    testList =
        [
            tN_takes_one_unnested_NC,
            tN_takes_most_unnested_NC_from_list_containing_two_complete_NCs,
            tN_returns_empty_list_if_called_on_only_unnested_complete_NC,
            tN_returns_next_NC_if_called_on_complete_NC_with_nest,
            tN_returns_nearest_complete_NC_if_called_on_incomplete_NC,
            tN_can_be_called_succesively_to_retrieve_nested_NCs,
            tDN_actually_returns_deepest_nest
        ]

tN_takes_one_unnested_NC = testCase name assertion where
    name      = "Takes one single NC list that is not nested and contains no nests"
    assertion = assertEqual d a f
    d         = "From 'def some(a,b)' return '(a,b)'"
    a         = "(a,b)"
    f         = takeNest ('(' == ) ( ')' == ) "def some(a,b)"

tN_takes_most_unnested_NC_from_list_containing_two_complete_NCs = testCase name assertion where
    name      = "The most unnested NC is taken"
    assertion = assertEqual d a f
    d         = "From 'def some(a,(b,c)):' return '(a,(b,c))'"
    a         = "(a,(b,c))"
    f         = takeNest ('(' == ) ( ')' == ) "def some(a,(b,c)):"

tN_returns_empty_list_if_called_on_only_unnested_complete_NC = testCase name assertion where
    name      = "takeNest returns empty list if called on a wholly complete NC with no nests"
    assertion = assertEqual d a f
    d         = "If called on the beginning of an NC, takeNest should return the next NC, if there is no NC, it should return nothing"
    a         = ""
    f         = takeNest ('(' == ) ( ')' == ) "(a, b)"

tN_returns_next_NC_if_called_on_complete_NC_with_nest = testCase name assertion where
    name      = "takeNest returns next NC if called on a wholly complete NC with one or more complete nests"
    assertion = assertEqual d a f
    d         = "If called on the beginning of an NC, takeNest should return the next NC, if there is no NC, it should return nothing"
    a         = "(b,c)"
    f         = takeNest ('(' == ) ( ')' == ) "(a, (b,c))"

tN_returns_nearest_complete_NC_if_called_on_incomplete_NC = testCase name assertion where
    name      = "takeNest returns nearest complete NC if called on a list that contains at least one complete NC"
    assertion = assertEqual d a f
    d         = "If called on the beginning of an NC, takeNest should return the next NC, if there is no NC, it should return nothing"
    a         = "(b,c)"
    f         = takeNest ('(' == ) ( ')' == ) "(a, (b,c)"

tN_can_be_called_succesively_to_retrieve_nested_NCs = testCase name assertion where
    name      = "takeNest can be used to retrieve varying depths of NC"
    assertion = assertEqual d a f
    d         = "NC can be nested to arbitrary levels, tNC should be able to retrieve them with successive calls"
    a         = "(7 + 8)"
    f         = takeNest ('(' == ) ( ')' == ) $ takeNest ('(' == ) ( ')' == ) $ takeNest ('(' == ) ( ')' == ) $ takeNest ('(' == ) ( ')' == ) "1 + (2 + (3 + 4 + ( 5 + 6 * (7 + 8)))))"

tDN_actually_returns_deepest_nest = testCase name assertion where
    name      = "takeDeepestNest returns the deepest nest"
    assertion = assertEqual d a f
    d         = "A function for painlessly retrieving the deepest nested NC"
    a         = "(7 + 8)"
    f         = takeDeepestNest ('(' == ) ( ')' == ) "1 + (2 + (3 + 4 + ( 5 + 6 * (7 + 8)))))"

-- | takeNextSameDepthNest as tSD
tSD_tests = testGroup "takeNextSameDepthNest test" testList where
    testList =
        [

        ]

tSD_returns_first_top_level_nest_if_first_element_not_a_nest = testCase name assertion where
    name      = "tSD returns the appropriate top-level nest"
    assertion = assertEqual d a f
    d         = "If the first elem of the given list is not a nest, then return the next complete nest"
    a         = "(def(ghi))"
    f         = takeSameDepth ('(' == ) ( ')' == ) "abc(def(ghi))lmn(opq)"