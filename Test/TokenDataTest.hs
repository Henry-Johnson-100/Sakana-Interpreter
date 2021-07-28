import Test.Tasty
import Test.Tasty.HUnit

import Token.Data

-- | main
main = do
    defaultMain tests

tests = testGroup "Token.Data tests" testList where
    testList =
        [
            consolidateStringsTests,
            readDataTests
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


-- | Token.Data.readData tests as rD
readDataTests = testGroup "readData" testList where
    testList =
        [
            rD_reads_null_string_to_Other,
            rD_reads_onlyDigits_string_as_Int,
            rD_reads_alphanumeric_string_as_Other,
            rD_reads_string_with_escaped_quote_as_String,
            rD_reads_string_true_to_Boolean_true,
            rD_reads_string_true_to_Boolean_false
        ]

rD_reads_null_string_to_Other = testCase name assertion where
    name      = "readData of null string input"
    assertion = assertEqual desc assert func
    desc      = "reading an empty string should return an empty string Other Data"
    assert    = Other ""
    func      = readData ""

rD_reads_onlyDigits_string_as_Int = testCase name assertion where
    name      = "readData of numeric string input"
    assertion = assertEqual desc assert func
    desc      = "any string consisting of only digits should return an Int Data"
    assert    = Int 56009
    func      = readData "56009"

rD_reads_alphanumeric_string_as_Other = testCase name assertion where
    name      = "an alphanumeric string is an Other, or undefined type"
    assertion = assertEqual d a f
    d         = "alphanumeric strings can not be data types so they should be read as Other"
    a         = Other "56aj90g"
    f         = readData "56aj90g"

rD_reads_string_with_escaped_quote_as_String = testCase name assertion where
    name      = "String with escaped quotes from a file are meant to be String Data"
    assertion = assertEqual d a f
    d         = "Strings are a data type in fish and when read from a file, they will contain an escaped quote"
    a         = String "\"Hello"
    f         = readData "\"Hello"

rD_reads_string_true_to_Boolean_true = testCase name assertion where
    name      = "Read a boolean Data from a string"
    assertion = assertEqual d a f
    d         = "Read a string with capitalized boolean to return a Boolean Data"
    a         = Boolean True
    f         = readData "True"

rD_reads_string_true_to_Boolean_false = testCase name assertion where
    name      = "Read a boolean Data from a string"
    assertion = assertEqual d a f
    d         = "Read a string with capitalized boolean to return a Boolean Data"
    a         = Boolean False
    f         = readData "False"