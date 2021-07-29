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
            cS_does_not_consolidate_incomplete_strings,
            cS_consolidates_one_string,
            cS_consolidates_two_strings_separately,
            cS_consolidates_three_strings_separately,
            cS_consolidates_string_containing_previously_identified_id_or_punct_or_other_types
        ]

cS_returns_empty_list_for_empty_arg = standardTimeout 5 $ testCase name assertion where
    name      = "An empty data list can not be consolidated"
    assertion = assertEqual desc assert func
    desc      = "consolidateStrings [] == []"
    assert    = []
    func      = consolidateStrings []

cS_does_not_consolidate_incomplete_strings = standardTimeout 5 $ testCase name assertion where
    name      = "If there is not a complete String EC in a data list, then nothing is consolidated"
    assertion = assertEqual desc assert func
    desc      = "Data lists with an incomplete String EC should not be consolidated since the string is not completed"
    assert    = [Int 5, String "\"beginning", Other "middle", Other "not", Other "the", Other "end", Boolean True]
    func      = consolidateStrings [Int 5, String "\"beginning", Other "middle", Other "not", Other "the", Other "end", Boolean True]

cS_consolidates_one_string = standardTimeout 5 $ testCase name assertion where
    name      = "Consolidate Data list containing one string"
    assertion = assertEqual desc assert func
    desc      = "For consolidateString containing one string collapsible instance ([String \"\"Hello \", Other \"dumb \", String \"world\"\"] return that instance consolidated into a list of Data.String(s)"
    assert    = [String "\"Hello dumb world\""]
    func      = consolidateStrings [String "\"Hello ", Other "dumb ", String "world\""]

cS_consolidates_two_strings_separately = standardTimeout 5 $ testCase name assertion where
    name      = "consolidate two or more separate instances of strings in a data list"
    assertion = assertEqual desc assert func
    desc      = "For consolidateString containing two string collapsible instances ([\"String \"\"Hello \", Other \"dumb \", String \"world\"\", Int 5, String \"\"Another \", Other \"dumb \", Other \"consolidate \", String \"test\"\"] return a list of [Data] appropriately consolidated)"
    assert    = [String "\"Hello dumb world\"", Int 5, String "\"Another dumb consolidate test\""]
    func      = consolidateStrings [String "\"Hello ", Other "dumb ", String "world\"", Int 5, String "\"Another ", Other "dumb ", Other "consolidate ", String "test\""]

cS_consolidates_three_strings_separately = standardTimeout 5 $ testCase name assertion where
    name      = "consolidate three or more separate instances of String EC's in a data list"
    assertion = assertEqual desc assert func
    desc      = "This test, in conjunction with the two preceding, should prove that consolidateStrings works for n number of complete String EC instances in a [Data]"
    assert    = [Int 5, String "\"stringonecase\"", Int 0, String "\"stringtwocase\"", Boolean False, String "\"stringthreecase\"", Boolean False, Boolean True]
    func      = consolidateStrings [Int 5, String "\"string", Other "one", String "case\"", Int 0, String "\"string", Other "two", String "case\"", Boolean False, String "\"string", Other "three", String "case\"", Boolean False, Boolean True]

cS_consolidates_string_containing_previously_identified_id_or_punct_or_other_types = standardTimeout 5 $ testCase name assertion where
    name      = "consolidate string still works for all data types inside a string EC"
    assertion = assertEqual d a f
    d         = "some data may be initially identified as Punct or Id, even if inside a String EC, when consolidated, this shouldn't matter"
    a         = [Int 5, Float 4.3, String "\"Please enter function name like: Some.factorial then try using a float like 4.3\"", Boolean False, Int 42]
    f         = consolidateStrings [Int 5, Float 4.3, String "\"Please ", Other "enter ", Id "function ", Id "name ", Other "like", Punct ":", Other " ", Id "Some.factorial", Other " ", Id "then ", Other "try ", Other "using ", Other "a ", Id "float ", Id "like ", String "4.3\"", Boolean False, Int 42 ]


-- | Token.Data.readData tests as rD
readDataTests = testGroup "readData" testList where
    testList =
        [
            rD_reads_null_string_to_Other,
            rD_reads_onlyDigits_string_as_Int,
            rD_reads_alphanumeric_string_as_Other,
            rD_reads_string_with_escaped_quote_as_String,
            rD_reads_string_true_to_Boolean_true,
            rD_reads_string_true_to_Boolean_false,
            rD_reads_simple_float,
            rD_reads_a_float_with_some_whitespace_padding,
            rD_reads_comma_as_Punct,
            rD_reads_comma_with_whitespace_padding_as_Punct,
            rD_reads_unidentified_alpha_strings_as_Id,
            rD_reads_snake_case_alpha_string_as_Id,
            rD_reads_alpha_string_containing_point_punctuation_as_id,
            rD_reads_alpha_snake_case_string_containing_point_punctuation_as_id,
            rD_reads_string_punct_numeric_string_as_Other,
            rD_reads_identifiable_data_type_as_String_if_beginning_or_ending_in_escaped_quote
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

rD_reads_simple_float = testCase name assertion where
    name      = "Read a simple float as a Float Data"
    assertion = assertEqual d a f
    d         = "A floating point string is a string of all digits and exactly one point char"
    a         = Float 5.0
    f         = readData "5.0"

rD_reads_a_float_with_some_whitespace_padding = testCase name assertion where
    name      = "a number with whitespace padding should still be typable"
    assertion = assertEqual d a f
    d         = "Read a number with leading and/or trailing whitespace"
    a         = Float 5.0
    f         = readData " 5.0 "

rD_reads_comma_as_Punct = testCase name assertion where
    name      = "Reading commas as punctuation type"
    assertion = assertEqual d a f
    d         = "Reading punctuation"
    a         = Punct ","
    f         = readData ","

rD_reads_comma_with_whitespace_padding_as_Punct = testCase name assertion where
    name      = "Reading commas as punctuation type"
    assertion = assertEqual d a f
    d         = "Reading punctuation"
    a         = Punct " , "
    f         = readData " , "

rD_reads_unidentified_alpha_strings_as_Id = testCase name assertion where
    name      = "for function naming"
    assertion = assertEqual d a f
    d         = "Identify words that aren't data types as ID's"
    a         = Id "factorial"
    f         = readData "factorial"

rD_reads_snake_case_alpha_string_as_Id = testCase name assertion where
    name      = "for function naming"
    assertion = assertEqual d a f
    d         = "should be able to name functions with snake case"
    a         = Id "some_func"
    f         = readData "some_func"

rD_reads_alpha_string_containing_point_punctuation_as_id = testCase name assertion where
    name      = "function call dot notation"
    assertion = assertEqual d a f
    d         = "potentially using dot function to call functions from other schools or modules"
    a         = Id "Some.factorial"
    f         = readData "Some.factorial"

rD_reads_alpha_snake_case_string_containing_point_punctuation_as_id = testCase name assertion where
    name      = "function call dot notation"
    assertion = assertEqual d a f
    d         = "potentially using dot function to call functions from other schools or modules"
    a         = Id "Some.some_func"
    f         = readData "Some.some_func"

rD_reads_string_punct_numeric_string_as_Other = testCase name assertion where
    name      = "Read string made up of only punctuation and digits"
    assertion = assertEqual d a f
    d         = "Read a strings of punctuation and digits as other type"
    a         = Other "5.89_6,4"
    f         = readData "5.89_6,4"

rD_reads_identifiable_data_type_as_String_if_beginning_or_ending_in_escaped_quote = testCase name assertion where
    name      = "read string numbers or other typable data as a string if beginning or end of quote"
    assertion = assertEqual d a f
    d         = "The string \"\"4.3\" should be a String and not identified as anything else"
    a         = String "\"4.3"
    f         = readData "\"4.3"