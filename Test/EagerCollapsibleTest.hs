module EagerCollapsibleTest(
    eagerCollapsibleTests
) where

--import Test.HUnit
import Test.Tasty
import Test.Tasty.HUnit
import Data.List

import Token.Util.EagerCollapsible as EC

-- | main
eagerCollapsibleTests = defaultMain tests

-- | All EC tests
tests = testGroup "Token.Util.EagerCollapsible Tests" testList where
    testList =
        [
            dropInfixTests,
            isECTests,
            tBTests
        ]


-- |Unit tests for Token.Util.EagerCollapsible.dropInfix
dropInfixTests = testGroup "dropInfix" testList where
    testList = [
        dropInfix_normal,
        dropInfix_not_infix,
        dropInfix_empty_infix,
        dropInfix_empty_list
        ]
dropInfix_normal = testCase name assertion where
    name      = "dropInfix happy params"
    assertion = assertEqual desc assert func
    desc      = "For dropInfix [2,3] [1,2,3,4] return [1,4]"
    assert    = [1,4]
    func      = (dropInfix [2,3] [1,2,3,4])
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



-- |Unit tests for Token.Util.EagerCollapsible.isEagerCollapsible as isEC
isECTests = testGroup "isEagerCollapsible" testList where
    testList = 
        [
            isEC_returns_true_for_EC,
            isEC_returns_false_for_not_EC,
            isEC_returns_false_for_single_string
        ]
isEC_returns_true_for_EC = testCase name assertion where
    name      = "isEC single EC in args"
    assertion = assertEqual desc assert func
    desc      = "return true for isEagerCollapsible (isPrefixOf \"\"\") (isSuffixOf \"\"\") [\"1\",\"\"Hello\",\" simple\", \" test\"\", \"1\"]"
    assert    = True
    func      = (isEagerCollapsible (isPrefixOf "\"") (isSuffixOf "\"") [" simple", " test\"","1"])
isEC_returns_false_for_not_EC = testCase name assertion where
    name      = "isEC with no EC"
    assertion = assertEqual desc assert func
    desc      = "return false for isEagerCollapsible (isPrefixOf \"\"\") (isSuffixOf \"\"\") [\"1\",\"\"Hello\",\"\" simple\", \" test\"\", \"1\"]"
    assert    = False
    func      = (isEagerCollapsible (isPrefixOf "\"") (isSuffixOf "\"") ["1","\"Hello", "\"simple", " test\"","1"])
isEC_returns_false_for_single_string = testCase name assertion where
    name      = "isEC with single elem EC"
    assertion = assertEqual desc assert func
    desc      = "return false for whether or not a single string: [\"Hello World\"] is an eager collapsible"
    assert    = False
    func      = (isEagerCollapsible (isPrefixOf "\"") (isSuffixOf "\"") ["\"Hello World\""])




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



