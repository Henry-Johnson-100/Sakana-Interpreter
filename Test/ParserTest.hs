import Parser.Core
import Syntax
import Test.Core

prepareParser p str = generalParse p "Test" str

main = defaultMain tests

tests =
  testGroup
    ""
    [dataParserTests]

dataParserTests =
  testGroup
    "Data Parser Tests"
    [ timedAssertEqual
        2
        "Parse a boolean"
        []
        (Syntax.Boolean True)
        (prepareParser dataParser "True"),
      timedAssertEqual
        2
        "Parse a False Boolean"
        []
        (Syntax.Boolean False)
        (prepareParser dataParser "False"),
      timedAssertEqual
        2
        "Parse a string literal with no whitespace."
        "Normal string with no extra whitespace (\\n \\t \\r) are parsed."
        (Syntax.String "Hello World!")
        (prepareParser dataParser "\"Hello World!\""),
      timedAssertEqual
        2
        "Parse a positive integer."
        []
        (Syntax.Num 100.0)
        (prepareParser dataParser "100"),
      timedAssertEqual
        2
        "Parse a negative integer."
        []
        (Syntax.Num (-100))
        (prepareParser dataParser "-100"),
      timedAssertEqual
        2
        "Parse a positive integer with a zero single digit decimal."
        []
        (Syntax.Num 100.0)
        (prepareParser dataParser "100.0"),
      timedAssertEqual
        2
        "Parse a positive double."
        []
        (Syntax.Num 100.12345)
        (prepareParser dataParser "100.12345"),
      timedAssertEqual
        2
        "Parse a negative double."
        []
        (Syntax.Num (-100.12345))
        (prepareParser dataParser "-100.12345"),
      timedAssertEqual
        2
        "Parse a normal \"main\" id."
        []
        (Syntax.Id "main")
        (prepareParser idParser "main"),
      timedAssertEqual
        2
        "Parse an operator id."
        []
        (Syntax.Id "+")
        (prepareParser idParser "+"),
      timedAssertEqual
        2
        "Parse a capitalized id."
        []
        (Syntax.Id "Main")
        (prepareParser idParser "Main"),
      timedAssertEqual
        2
        "Parse a normal id with symbols in it."
        []
        (Syntax.Id "mai%n")
        (prepareParser idParser "mai%n"),
      timedAssertEqual
        2
        "Parse a normal id with a capitalized accessor prefix."
        []
        (Syntax.Id "Main.main")
        (prepareParser idParser "Main.main"),
      timedAssertEqual
        2
        "Parser an operator id with a capitalized accessor prefix."
        []
        (Syntax.Id "Main.+")
        (prepareParser idParser "Main.+"),
      timedAssertEqual
        2
        "Parse a normal id with a normal accessor prefix."
        []
        (Syntax.Id "normal.main")
        (prepareParser idParser "normal.main"),
      timedAssertEqual
        2
        "Parse a normal id with an operator accessor prefix."
        []
        (Syntax.Id "+.main")
        (prepareParser idParser "+.main")
    ]