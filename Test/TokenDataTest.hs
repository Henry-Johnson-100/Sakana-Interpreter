import Test.Tasty
import Test.Tasty.HUnit

import Token.Data

-- | main
main = do
    defaultMain tests

tests = testGroup "Token.Data tests" testList where
    testList =
        [
            consolidateStringsTests
        ]

standardTimeout timeS= localOption (Timeout (timeS * 1000000) (concat [show timeS, "s"]))

-- | Token.Data.consolidateString tests as cS
consolidateStringsTests = testGroup "Token.Data.consolidateString tests" testList where
    testList =
        [
            cS_returns_empty_list_for_empty_arg,
            cS_consolidates_one_string,
            cS_consolidates_two_strings_separately,
            cS_consolidates_three_strings_separately,
            cS_does_not_consolidate_incomplete_strings
        ]

cS_returns_empty_list_for_empty_arg = standardTimeout 5 $ testCase name assertion where
    name      = "An empty data list can not be consolidated"
    assertion = assertEqual desc assert func
    desc      = "consolidateStrings [] == []"
    assert    = []
    func      = consolidateStrings []

cS_consolidates_one_string = standardTimeout 5 $ testCase name assertion where
    name      = "Consolidate Data list containing one string"
    assertion = assertEqual desc assert func
    desc      = "For consolidateString containing one string collapsible instance ([String \"\"Hello \", Other \"dumb \", String \"world\"\"] return that instance consolidated into a list of Data.String(s)"
    assert    = [String "\"Hello ", String "dumb ", String "world\""]
    func      = consolidateStrings [String "\"Hello ", Other "dumb ", String "world\""]

cS_consolidates_two_strings_separately = standardTimeout 5 $ testCase name assertion where
    name      = "consolidate two or more separate instances of strings in a data list"
    assertion = assertEqual desc assert func
    desc      = "For consolidateString containing two string collapsible instances ([\"String \"\"Hello \", Other \"dumb \", String \"world\"\", Int 5, String \"\"Another \", Other \"dumb \", Other \"consolidate \", String \"test\"\"] return a list of [Data] appropriately consolidated)"
    assert    = [String "\"Hello ", String "dumb ", String "world\"", Int 5, String "\"Another ", String "dumb ", String "consolidate ", String "test\""]
    func      = consolidateStrings [String "\"Hello ", Other "dumb ", String "world\"", Int 5, String "\"Another ", Other "dumb ", Other "consolidate ", String "test\""]

cS_consolidates_three_strings_separately = standardTimeout 5 $ testCase name assertion where
    name      = "consolidate three or more separate instances of String EC's in a data list"
    assertion = assertEqual desc assert func
    desc      = "This test, in conjunction with the two preceding, should prove that consolidateStrings works for n number of complete String EC instances in a [Data]"
    assert    = [Int 5, String "\"string", String "one", String "case\"", Int 0, String "\"string", String "two", String "case\"", Boolean False, String "\"string", String "three", String "case\"", Boolean False, Boolean True]
    func      = consolidateStrings [Int 5, String "\"string", Other "one", String "case\"", Int 0, String "\"string", Other "two", String "case\"", Boolean False, String "\"string", Other "three", String "case\"", Boolean False, Boolean True]

cS_does_not_consolidate_incomplete_strings = standardTimeout 5 $ testCase name assertion where
    name      = "If there is not a complete String EC in a data list, then nothing is consolidated"
    assertion = assertEqual desc assert func
    desc      = "Data lists with an incomplete String EC should not be consolidated since the string is not completed"
    assert    = [Int 5, String "\"beginning", Other "middle", Other "not", Other "the", Other "end", Boolean True]
    func      = consolidateStrings [Int 5, String "\"beginning", Other "middle", Other "not", Other "the", Other "end", Boolean True]