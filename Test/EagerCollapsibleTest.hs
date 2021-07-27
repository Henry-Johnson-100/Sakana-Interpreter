module Test.EagerCollapsibleTest(
main
) where

import Test.HUnit
import Data.List

import Token.Util.EagerCollapsible as EC

-- |Unit tests for Token.Util.EagerCollapsible.dropInfix
dropInfix_normal = TestCase $ assertEqual "For dropInfix [2,3] [1,2,3,4] return [1,4]" [1,4] (dropInfix [2,3] [1,2,3,4])

dropInfix_not_infix = TestCase $ assertEqual "For dropInfix [2,4] [1,2,3,4] return [1,2,3,4]" [1,2,3,4] (dropInfix [2,4] [1,2,3,4])

dropInfix_empty_infix = TestCase $ assertEqual "For dropInfix [] [a] return [a]" [1,2,3,4] (dropInfix [] [1,2,3,4])

dropInfix_empty_list = TestCase $ assertEqual "For dropInfix _ [] return []" [] (dropInfix [2,3] [])

dropInfix_testList = TestList [dropInfix_normal, dropInfix_not_infix, dropInfix_empty_infix, dropInfix_empty_list]

dropInfixTests = runTestTT dropInfix_testList


-- |Unit tests for Token.Util.EagerCollapsible.isEagerCollapsible as isEC
isEC_returns_true_for_EC = TestCase $ assertEqual "return true for isEagerCollapsible (isPrefixOf \"\"\") (isSuffixOf \"\"\") [\"1\",\"\"Hello\",\" simple\", \" test\"\", \"1\"]" True (isEagerCollapsible (isPrefixOf "\"") (isSuffixOf "\"") [" simple", " test\"","1"])

isEC_returns_false_for_not_EC = TestCase $ assertEqual "return false for isEagerCollapsible (isPrefixOf \"\"\") (isSuffixOf \"\"\") [\"1\",\"\"Hello\",\"\" simple\", \" test\"\", \"1\"]" False (isEagerCollapsible (isPrefixOf "\"") (isSuffixOf "\"") ["1","\"Hello", "\"simple", " test\"","1"])

isEC_returns_false_for_single_string = TestCase $ assertEqual "return false for whether or not a single string: [\"Hello World\"] is an eager collapsible" False (isEagerCollapsible (isPrefixOf "\"") (isSuffixOf "\"") ["\"Hello World\""])

isEC_testList = TestList [isEC_returns_true_for_EC, isEC_returns_false_for_not_EC,isEC_returns_false_for_single_string]

isECTests = runTestTT isEC_testList


-- | Unit tests for Token.Util.EagerCollapsible.takeBetween as tB
tB_returns_empty_list_for_empty_list_argument = TestCase $ assertEqual "For takeBetween _ _ [] return []" [] (takeBetween (1==) (1==) [])

tB_returns_inclusive_list_of_EC = TestCase $ assertEqual "for an EC with beginCase (==1) and endCase (==1), return inclusive list of all elements between the two." ([1,2,3,4,5,1]) (takeBetween ((==) 1) ((==) 1) [2,3,1,2,3,4,5,1,2,3,4])

tB_returns_only_one_EC_from_list_with_multiple_separate_EC = TestCase $ assertEqual "for takeBetween (1==) (1==) [2,1,2,3,1,2,1,8,9,7,1,3] return only the first inclusive EC: [1,2,3,1]" [1,2,3,1] (takeBetween (1==) (1==) [2,1,2,3,1,2,1,8,9,7,1,3])

tBTests = runTestTT $ TestList [tB_returns_empty_list_for_empty_list_argument, tB_returns_inclusive_list_of_EC,tB_returns_only_one_EC_from_list_with_multiple_separate_EC]



-- | run all tests for Token.Util.EagerCollapsible
main = do
    dropInfixTests
    isECTests
    tBTests