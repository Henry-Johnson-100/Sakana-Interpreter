module TokenDataTest(
    dataTests
) where

import Test.HUnit

import Token.Data

-- | Token.Data.consolidateString tests as cS
cS_consolidates_one_string = TestCase $ assertEqual "For consolidateString containing one string collapsible instance ([String \"\"Hello \", Other \"dumb \", String \"world\"\"] return that instance consolidated into a list of Data.String(s)" ([String "\"Hello ", String "dumb ", String "world\""]) (consolidateStrings [String "\"Hello ", Other "dumb ", String "world\""])
cS_consolidates_two_strings_separately = TestCase $ assertEqual message assert func where
    message = "For consolidateString containing two string collapsible instances ([\"String \"\"Hello \", Other \"dumb \", String \"world\"\", Int 5, String \"\"Another \", Other \"dumb \", Other \"consolidate \", String \"test\"\"] return a list of [Data] appropriately consolidated)"
    assert = [String "\"Hello ", String "dumb ", String "world\"", Int 5, String "\"Another ", String "dumb ", String "consolidate ", String "test\""]
    func = consolidateStrings [String "\"Hello ", Other "dumb ", String "world\"", Int 5, String "\"Another ", Other "dumb ", Other "consolidate ", String "test\""]
cS_testList = TestList [cS_consolidates_one_string, cS_consolidates_two_strings_separately]
cS_tests = runTestTT cS_testList

dataTests = do
    cS_tests