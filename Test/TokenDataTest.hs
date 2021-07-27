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


-- | Token.Data.consolidateString tests as cS
consolidateStringsTests = testGroup "Token.Data.consolidateString tests" testList where
    testList =
        [
            cS_consolidates_one_string,
            cS_consolidates_two_strings_separately
        ]
cS_consolidates_one_string = (localOption (Timeout (5000000) "10s")) $ testCase name assertion where
    name      = "Consolidate Data list containing one string"
    assertion = assertEqual desc assert func
    desc      = "For consolidateString containing one string collapsible instance ([String \"\"Hello \", Other \"dumb \", String \"world\"\"] return that instance consolidated into a list of Data.String(s)"
    assert    = [String "\"Hello ", String "dumb ", String "world\""]
    func      = consolidateStrings [String "\"Hello ", Other "dumb ", String "world\""]
cS_consolidates_two_strings_separately = (localOption (Timeout (5000000) "10s")) $ testCase name assertion where
    name      = "consolidate two or more separate instances of strings in a data list"
    assertion = assertEqual desc assert func
    desc      = "For consolidateString containing two string collapsible instances ([\"String \"\"Hello \", Other \"dumb \", String \"world\"\", Int 5, String \"\"Another \", Other \"dumb \", Other \"consolidate \", String \"test\"\"] return a list of [Data] appropriately consolidated)"
    assert    = [String "\"Hello ", String "dumb ", String "world\"", Int 5, String "\"Another ", String "dumb ", String "consolidate ", String "test\""]
    func      = consolidateStrings [String "\"Hello ", Other "dumb ", String "world\"", Int 5, String "\"Another ", Other "dumb ", Other "consolidate ", String "test\""]