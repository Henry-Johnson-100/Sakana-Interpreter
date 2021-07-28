import Test.Tasty
import Test.Tasty.HUnit
import Data.List

import Token.Util.EagerCollapsible as EC

-- | main
main = do
    defaultMain tests

-- | All EC tests
tests = testGroup "Token.Util.EagerCollapsible Tests" testList where
    testList =
        [
            isECTests,
            dropInfixTests,
            tBTests,
            dBTests
        ]



-- |Unit tests for Token.Util.EagerCollapsible.isEagerCollapsible as isEC
isECTests = testGroup "isEagerCollapsible" testList where
    testList = 
        [
            isEC_returns_false_for_empty_list,
            isEC_returns_true_for_list_containing_EC_regardless_of_initial_index,
            isEC_returns_true_for_list_containing_EC_regardless_of_initial_index_two,
            isEC_returns_false_for_list_not_containing_EC_regardless_of_initial_index,
            isEC_returns_false_if_encounters_incomplete_EC_before_complete_EC,
            isEC_returns_true_if_encounters_complete_EC_before_any_other_beginCase,
            isEC_returns_false_for_single_string,
            isEC_returns_false_for_single_element_matching_both_terminal_cases,
            isEC_returns_true_for_single_EC_with_different_terminal_cases
        ]

isEC_returns_false_for_empty_list = testCase name assertion where
    name      = "return false for empty list"
    assertion = assertEqual desc assert func
    desc      = "Empty lists cannot contain an EC instance"
    assert    = False
    func      = isEagerCollapsible (1==) (5==) []

isEC_returns_true_for_list_containing_EC_regardless_of_initial_index = testCase name assertion where
    name      = "accurate boolean for isEagerCollapsible regardless of initial index position"
    assertion = assertEqual desc assert func
    desc      = "isEagerCollapsible should return True for any list containing anEagerCollapsible (as defined in the first two args) regardless of where it appears in the list"
    assert    = True
    func      = isEagerCollapsible (isPrefixOf "\"") (isSuffixOf "\"") ["not the start", "\"this is ", "the middle ", "this is ", "the end\"", "not part", "of the EC"]

isEC_returns_true_for_list_containing_EC_regardless_of_initial_index_two = testCase name assertion where
    name      = "accurate boolean for isEagerCollapsible regardless of initial index position"
    assertion = assertEqual desc assert func
    desc      = "isEagerCollapsible should return True for any list containing an EagerCollapsible (as defined in the first two args) regardless of where it appears in the list"
    assert    = True
    func      = isEagerCollapsible (1==) (5==) [1,2,3,4,5,6]

isEC_returns_false_for_list_not_containing_EC_regardless_of_initial_index = testCase name assertion where
    name      = "list not containing a complete EC should return false, regardless of the initial index and \n(at least one of the terminal conditions match)"
    assertion = assertEqual desc assert func
    desc      = "isEagerCollapsible (isPrefixOf \"\\\"\") (isSuffixOf \"\\\"\") [\"not \", \"A \", \"complete \", \"EC \\\"\", \"extra string\"] == False"
    assert    = False
    func      = isEagerCollapsible (isPrefixOf "\"") (isSuffixOf "\"") ["not ", "a ", "complete ", "EC\"", "extra string"]

isEC_returns_false_if_encounters_incomplete_EC_before_complete_EC = testCase name assertion where
    name      = "isEagerCollapsible should only return True if a complete EC is found before an incomplete EC"
    assertion = assertEqual desc assert func
    desc      = "if endCase encountered before beginCase, return False"
    assert    = False
    func      = isEagerCollapsible (0==) (9==) [5,6,7,8,9,0,1,2,3,4,5,9,3,4,5]

isEC_returns_true_if_encounters_complete_EC_before_any_other_beginCase = testCase name assertion where
    name      = "isEagerCollapsible returns True a complete EC occurs before any other partial or complete EC"
    assertion = assertEqual desc assert func
    desc      = "if beginCase and endCase are encountered before another complete or partial EC, return True"
    assert    = True
    func      = isEagerCollapsible (0==) (9==) [1,2,3,0,5,6,7,8,9,5,6,4,9,3,2,0,1,9]

isEC_returns_false_for_single_string = testCase name assertion where
    name      = "isEC with single elem EC"
    assertion = assertEqual desc assert func
    desc      = "return false for whether or not a single string: [\"Hello World\"] is an eager collapsible"
    assert    = False
    func      = (isEagerCollapsible (isPrefixOf "\"") (isSuffixOf "\"") ["\"Hello World\""])

isEC_returns_false_for_single_element_matching_both_terminal_cases = testCase name assertion where
    name      = "A single element list matching both terminal cases should return False"
    assertion = assertEqual desc assert func
    desc      = "isEagerCollapsible (1==) (1==) [1] == False"
    assert    = False
    func      = isEagerCollapsible (1==) (1==) [1]

isEC_returns_true_for_single_EC_with_different_terminal_cases = testCase name assertion where
    name      = "single EC with different terminal cases"
    assertion = assertEqual desc assert func
    desc      = "isEagerCollapsible (1==) (5==) [2,3,1,2,4,6,5,2] == True"
    assert    = True
    func      = isEagerCollapsible (1==) (5==) [2,3,1,2,4,6,5,2]


-- |Unit tests for Token.Util.EagerCollapsible.dropInfix
dropInfixTests = testGroup "dropInfix" testList where
    testList = [
        dropInfix_normal,
        dropInfix_normal_two,
        dropInfix_not_infix,
        dropInfix_empty_infix,
        dropInfix_empty_list,
        dropInfix_drops_nearest_complete_infix_from_list_of_repeating_infix_patterns,
        dropInfix_drops_only_first_complete_infix_from_list,
        dropInfix_drops_nearest_complete_infix_from_list_of_repeating_infix_patterns_two
        ]
dropInfix_normal = testCase name assertion where
    name      = "dropInfix happy params"
    assertion = assertEqual desc assert func
    desc      = "For dropInfix [2,3] [1,2,3,4] return [1,4]"
    assert    = [1,4]
    func      = (dropInfix [2,3] [1,2,3,4])

dropInfix_normal_two = testCase name assertion where
    name      = "dropInfix works for longer lists with a single infix"
    assertion = assertEqual desc assert func
    desc      = "dropInfix [1,2,3,1] [4,5,1,2,3,1,4,5,6] == [4,5,4,5,6]"
    assert    = [4,5,4,5,6]
    func      = dropInfix [1,2,3,1] [4,5,1,2,3,1,4,5,6]

dropInfix_not_infix = testCase name assertion where
    name      = "dropInfix a b, a not infix"
    assertion = assertEqual desc assert func
    desc      = "For dropInfix [2,4] [1,2,3,4] return [1,2,3,4]"
    assert    = [1,2,3,4]
    func      = (dropInfix [2,4] [1,2,3,4])

dropInfix_empty_infix = testCase name assertion where
    name      = "dropInfix empty infix"
    assertion = assertEqual desc assert func
    desc      = "For dropInfix [] [a] return [a]"
    assert    = [1,2,3,4]
    func      = (dropInfix [] [1,2,3,4])

dropInfix_empty_list = testCase name assertion where
    name      = "dropInfix empty list"
    assertion = assertEqual desc assert func
    desc      = "For dropInfix _ [] return []"
    assert    = []
    func      = (dropInfix [2,3] [])

dropInfix_drops_nearest_complete_infix_from_list_of_repeating_infix_patterns = testCase name assertion where
    name      = "drops infix from list of repeating patterns"
    assertion = assertEqual desc assert func
    desc      = "dropInfix drops the nearest complete infix from a list of repeating infixes: dropInfix [1,2,3,1] [2,3,1,4,1,2,3,1,4,1,2,3] == [2,3,1,4,4,1,2,3,1]"
    assert    = [2,3,1,4,4,1,2,3]
    func      = dropInfix [1,2,3,1] [2,3,1,4,1,2,3,1,4,1,2,3]

dropInfix_drops_only_first_complete_infix_from_list = testCase name assertion where
    name      = "Drops only first complete infix"
    assertion = assertEqual desc assert func
    desc      = "For lists with multiple infix matches, drop only the first complete infix"
    assert    = [2,3,1,4,4,1,2,3,1,5]
    func      = dropInfix [1,2,3,1] [2,3,1,4,1,2,3,1,4,1,2,3,1,5]

dropInfix_drops_nearest_complete_infix_from_list_of_repeating_infix_patterns_two = testCase name assertion where
    name      = "drops infix from list of repeating patterns"
    assertion = assertEqual desc assert func
    desc      = "dropInfix drops the nearest complete infix from a list of repeating infixes: dropInfix [1,2,3,1] [2,3,1,4,1,2,3,1,5] == [2,3,1,4,5]"
    assert    = [2,3,1,4,5]
    func      = dropInfix [1,2,3,1] [2,3,1,4,1,2,3,1,5]


-- | Unit tests for Token.Util.EagerCollapsible.takeBetween as tB
tBTests = testGroup "takeBetween" testList where
    testList = 
        [
            tB_returns_empty_list_for_empty_list_argument,
            tB_returns_inclusive_list_of_EC,
            tB_returns_only_one_EC_from_list_with_multiple_separate_EC
        ]
tB_returns_empty_list_for_empty_list_argument = testCase name assertion where
    name = "tB empty list argument"
    assertion = assertEqual desc assert func
    desc = "For takeBetween _ _ [] return []"
    assert = []
    func = (takeBetween (1==) (1==) [])

tB_returns_inclusive_list_of_EC = testCase name assertion where
    name = "Return takeBetween list of EC list with one EC"
    assertion = assertEqual desc assert func
    desc = "for an EC with beginCase (==1) and endCase (==1), return inclusive list of all elements between the two."
    assert = [1,2,3,4,5,1]
    func = takeBetween ((==) 1) ((==) 1) [2,3,1,2,3,4,5,1,2,3,4]

tB_returns_only_one_EC_from_list_with_multiple_separate_EC = testCase name assertion where
    name = "One takeBetween from EC list with two or more EC"
    assertion = assertEqual desc assert func
    desc = "for takeBetween (1==) (1==) [2,1,2,3,1,2,1,8,9,7,1,3] return only the first inclusive EC: [1,2,3,1]"
    assert = [1,2,3,1]
    func = (takeBetween (1==) (1==) [2,1,2,3,1,2,1,8,9,7,1,3])


-- | Unit tests for Token.Util.EagerCollapsible.dropBetween as dB
dBTests = testGroup "dropBetween" testList where
    testList =
        [
            dB_returns_empty_list_for_empty_list_arg,
            dB_returns_list_with_single_EC_dropped,
            dB_intermediate_tB_list_is_expected,
            dB_returns_list_of_two_or_more_EC_with_only_one_EC_dropped
        ]
dB_returns_empty_list_for_empty_list_arg = testCase name assertion where
    name      = "dropBetween _ _ []"
    assertion = assertEqual desc assert func
    desc      = "for any dropBetween _ _ [], return []"
    assert    = []
    func      = dropBetween (1==) (1==) []

dB_returns_list_with_single_EC_dropped = testCase name assertion where
    name      = "for list xs with one EC, return a list with that EC (inclusive) dropped"
    assertion = assertEqual desc assert func
    desc      = "dropBetween (1==) (1==) [3,1,2,3,1,4] == [3,4]"
    assert    = [3,4]
    func      = dropBetween (1==) (1==) [3,1,2,3,1,4]

dB_intermediate_tB_list_is_expected = testCase name assertion where
    name      = "Determine that an intermediate value in dropBetween is what is expected from takeBetween"
    assertion = assertEqual desc assert func
    desc      = "dropBetween (1==) (1==) [3,1,2,3,1,4] == dropInfix ([1,2,3,1]) ([3,1,2,3,1,4])"
    assert    = dropInfix ([1,2,3,1]) ([3,1,2,3,1,4])
    func      = dropBetween (1==) (1==) [3,1,2,3,1,4]

dB_returns_list_of_two_or_more_EC_with_only_one_EC_dropped = testCase name assertion where
    name      = "for list xs with two or more EC, drop only the first EC (inclusive)"
    assertion = assertEqual desc assert func
    desc      = "dropBetween (1==) (1==) [2,3,1,2,3,4,1,4,5,1,2,3,1,6,7]"
    assert    = [2,3,4,5,1,2,3,1,6,7]
    func      = dropBetween (1==) (1==) [2,3,1,2,3,4,1,4,5,1,2,3,1,6,7]

