import Test.Tasty
import Test.Tasty.HUnit
import Data.List

import Token.Util.NestedCollapsible as NC

main = do
    defaultMain tests

testTerminals = NCCase ('('==) (')'==)

standardTimeout timeS= localOption (Timeout (timeS * 1000000) (concat [show timeS, "s"]))

nestPartitionLaw :: NCCase a -> [a] -> Bool
nestPartitionLaw nc xs = ((partFst part) ++ (partSnd part) ++ (partThd part)) == xs where
    part = breakByNest nc xs

tests = testGroup "Token.Util.NestedCollapsible Tests" testList where
    testList =
        [
            iCNC_tests,
            hNC_tests,
            partition_tests
        ]

-- | isCompleteNestedCollapsible Tests as iCNC
iCNC_tests = testGroup "Token.Util.NestedCollapsible.isCompleteNestedCollapsible Tests" testList where
    testList =
        [
            iCNC_returns_false_for_empty_list,
            iCNC_identifies_a_list_with_one_complete_unnested_NC,
            iCNC_identifies_a_list_with_one_nested_NC,
            iCNC_returns_false_for_a_list_with_mismatched_terminal_counts,
            iCNC_returns_false_for_list_containing_only_complete_NC_but_starting_and_ending_with_distinct_NC
        ]

iCNC_returns_false_for_empty_list = standardTimeout 5 $ testCase name assertion where
    name      = "Empty list returns False"
    assertion = assertEqual d a f
    d         = "An empty list contains no nested collapsibles"
    a         = False
    f         = isCompleteNestedCollapsible testTerminals ""

iCNC_identifies_a_list_with_one_complete_unnested_NC = standardTimeout 5 $ testCase name assertion where
    name      = "A list consisting wholly of a single complete NC returns true" 
    assertion = assertEqual d a f
    d         = "(a,b)"
    a         = True
    f         = isCompleteNestedCollapsible testTerminals "(a,b)"

iCNC_identifies_a_list_with_one_nested_NC = standardTimeout 5 $ testCase name assertion where
    name      = "isCompleteNestedCollapsible returns True for a list with an NC nested in another NC"
    assertion = assertEqual d a f
    d         = "NestedCollapsibles can contain nested collapsibles"
    a         = True
    f         = isCompleteNestedCollapsible testTerminals "(a,(dfjgj),sdjk)"

iCNC_returns_false_for_a_list_with_mismatched_terminal_counts = standardTimeout 5 $ testCase name assertion where
    name      = "returns False for a list with mismatched number of terminal counts"
    assertion = assertEqual d a f
    d         = "def some_func(a, (bc):"
    a         = False
    f         = isCompleteNestedCollapsible testTerminals "(a, (bc)"

iCNC_returns_false_for_list_containing_only_complete_NC_but_starting_and_ending_with_distinct_NC = standardTimeout 5 $ testCase name assertion where
    name      = "isCompleteNestedCollapsible should return True if and only if the entire given list is one complete NC and not multiple complete NC's"
    assertion = assertEqual d a f
    d         = "This function is used only to detect if the given list is one single unified complete NC"
    a         = False
    f         = isCompleteNestedCollapsible testTerminals "(def(ghi))lmn(opq)"

-- | hasNestedCollapsible Tests as hNC
hNC_tests = testGroup "Token.Util.NestedCollapsible.hasNestedCollapsible Tests" testList where
    testList =
        [
            hNC_returns_true_for_list_with_one_complete_and_one_incomplete_NC
        ]

hNC_returns_true_for_list_with_one_complete_and_one_incomplete_NC = standardTimeout 5 $ testCase name assertion where
    name      = "A list with one complete and one incomplete NC returns True"
    assertion = assertEqual d a f
    d         = "hasNestedCollapsible is a more general function, that doesn't tell you if the first encountered NC is complete, just if there is a complete NC at some point."
    a         = True        
    f         = hasNestedCollapsible testTerminals "def some_func(a, (bc):"

partition_tests = testGroup "breakByNest tests" testList where
    testList =
        [
            part_one,
            part_two,
            part_three
        ]

part_one = testCase name assertion where
    name      = "partition law test"
    assertion = assertEqual d a f
    d         = "partition law states that the three parts of the partition should always be equal to the original list"
    a         = True
    f         = nestPartitionLaw testTerminals "some (a,b) test (gh,g(fghng)) (gjhf)"

part_two = testCase name assertion where
    name      = "partition law test"
    assertion = assertEqual d a f
    d         = "partition law states that the three parts of the partition should always be equal to the original list"
    a         = True
    f         = nestPartitionLaw testTerminals "(some (a,bgd) hjf(dkfgk)) fhjkd (fhjkfg)"

part_three = testCase name assertion where
    name      = "partition law test"
    assertion = assertEqual d a f
    d         = "partition law states that the three parts of the partition should always be equal to the original list"
    a         = True
    f         = nestPartitionLaw testTerminals "(fhjfksds  (fgjksd (fgjsk (gjrt( jsd)))))"