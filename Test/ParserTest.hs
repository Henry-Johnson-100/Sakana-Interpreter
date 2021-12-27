import Parser.Core
import Syntax
import Test.Core
import Util.Tree

prepareParser p str = generalParse p "Test" str

main = defaultMain tests

tests =
  testGroup
    ""
    [ dataParserTests,
      keywordParserTests,
      functionCallParserTests,
      lampreyAndFunctionParserTests
    ]

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
        (prepareParser idParser "+.main"),
      timedAssertEqual
        2
        "Parse \"fish\" as an id."
        []
        (Syntax.Id "fish")
        (prepareParser idParser "fish")
    ]

keywordParserTests =
  testGroup
    "Keyword Parser Tests"
    [ timedAssertEqual
        2
        "Parse a fish keyword."
        []
        (Syntax.Fish)
        (prepareParser (genericKeywordParser Syntax.Fish) "fish"),
      timedAssertEqual
        2
        "Parse a shoal keyword."
        []
        (Syntax.Shoal)
        (prepareParser (genericKeywordParser Syntax.Shoal) "shoal"),
      timedAssertEqual
        2
        "Parse a lamprey keyword."
        []
        (Syntax.Lamprey)
        (prepareParser (genericKeywordParser Syntax.Lamprey) "lamprey")
    ]

functionCallParserTests =
  testGroup
    "Function Call Parser Tests"
    [ timedAssertEqual
        2
        "Parse a simple function call."
        []
        [treeId Return "or" -<= (listIds Send ["x", "y"])]
        (prepareParser (functionCallParser Return) "or >(x)> >(y)>")
    ]

lampreyAndFunctionParserTests =
  testGroup
    "Lamprey and FunctionDefinition Parser Tests"
    [ timedAssertEqual
        2
        "Parse a simple Lamprey."
        []
        [ treeKeyword Return Lamprey
            -<= [ treeId Send "x",
                  treeId Send "y",
                  treeId Return "+"
                    -<= [ treeId Send "x",
                          treeId Send "y"
                        ]
                ]
        ]
        ( prepareParser
            (lampreyParser Syntax.Return)
            "lamprey >(x)> >(y)> <(+ >(x)> >(y)>)<"
        ),
      timedAssertEqual
        2
        "Parse a simple function definition."
        []
        [ treeKeyword Return Fish
            -<= [ treeId Return "add"
                    -<= [ treeKeyword Return Lamprey
                            -<= [ treeId Send "x",
                                  treeId Send "y",
                                  treeId Return "+"
                                    -<= [ treeId Send "x",
                                          treeId Send "y"
                                        ]
                                ]
                        ]
                ]
        ]
        ( prepareParser
            (functionDefinitionParser Syntax.Return)
            "fish add lamprey\
            \ >(x)> >(y)> <(+ >(x)> >(y)>)<"
        ),
      timedAssertEqual
        2
        "A lamprey does not need an explicit keyword."
        []
        [ treeKeyword Return Lamprey
            -<= [ treeId Send "x",
                  treeId Send "y",
                  treeId Return "+"
                    -<= [ treeId Send "x",
                          treeId Send "y"
                        ]
                ]
        ]
        ( prepareParser
            (lampreyParser Return)
            ">(x)> \
            \>(y)> \
            \<(+ >(x)> >(y)>)<"
        ),
      -- LAMPREY WITH DEFINED CO-FUNCTION AS PARAMETER.
      timedAssertEqual
        2
        "Parse a lamprey with a scoped function definition."
        []
        [ treeKeyword Syntax.Return Lamprey
            -<= [ treeId Send "x",
                  treeKeyword Send Fish
                    -<= [ treeId Send "sub"
                            -- It makes sense that this is Return because calling the
                            -- function's id should return this lambda function.
                            -<= [ treeKeyword Return Lamprey
                                    -<= [ treeId Send "x",
                                          treeId Send "y",
                                          treeId Return "-"
                                            -<= (listIds Send ["x", "y"])
                                        ]
                                ]
                        ],
                  -- The 'x' and 'y' are parsed as Return but they should definitely
                  -- be Send
                  treeId Return "+" -<= (listIds Send ["x", "y"])
                ]
        ]
        ( prepareParser
            (lampreyParser Syntax.Return)
            "lamprey\
            \ >(x)>\
            \ >(fish sub >(x)> >(y)> <(- >(x)> >(y)>)<)>\
            \ <(+ >(x)> >(y)>)<"
        ),
      timedAssertEqual
        2
        "Parse a function definition that returns an explicit lamprey."
        []
        [ treeKeyword Return Fish
            -<= [ treeId Return "return_part"
                    -<= [ treeKeyword Return Lamprey
                            -<= [ treeId Send "x",
                                  treeKeyword Return Lamprey
                                    -<= [ treeId Send "y",
                                          treeId Return "+"
                                            -<= [ treeId Send "x",
                                                  treeId Send "sqrt"
                                                    -<= [treeId Send "y"]
                                                ]
                                        ]
                                ]
                        ]
                ]
        ]
        ( prepareParser
            (functionDefinitionParser Return)
            "fish return_part \
            \lamprey \
            \>(x)> \
            \<( lamprey \
            \    >(y)> \
            \    <(+ >(x)> >(sqrt >(y)>)>)< \
            \)<"
        ),
      timedAssertEqual
        2
        "Parse a function definition that returns an implicit lamprey"
        []
        [ treeKeyword Return Fish
            -<= [ treeId Return "return_part"
                    -<= [ treeKeyword Return Lamprey
                            -<= [ treeId Send "x",
                                  treeKeyword Return Lamprey
                                    -<= [ treeId Send "y",
                                          treeId Return "+"
                                            -<= [ treeId Send "x",
                                                  treeId Send "sqrt"
                                                    -<= [treeId Send "y"]
                                                ]
                                        ]
                                ]
                        ]
                ]
        ]
        ( prepareParser
            (functionDefinitionParser Return)
            "fish return_part \
            \lamprey \
            \>(x)> \
            \<( \
            \    >(y)> \
            \    <(+ >(x)> >(sqrt >(y)>)>)< \
            \)<"
        ),
      timedAssertEqual
        2
        "Parse a function that contains a swim block."
        []
        [ treeKeyword Return Fish
            -<= [ treeId Return "do_swim"
                    -<= [ treeId Send "x"
                            -<= [ treeKeyword Return Swim
                                    -<= [ treeId Send "bind_this"
                                            -<= [treeId Return "x"],
                                          treeId Send "trout"
                                            -<= [ treeId Send "to_string"
                                                    -<= [treeId Send "bind_this"]
                                                ],
                                          treeId Return "+"
                                            -<= [ (treeSU Send . dataNum) 1,
                                                  treeId Send "bind_this"
                                                ]
                                        ]
                                ]
                        ]
                ]
        ]
        ( prepareParser
            (functionDefinitionParser Return)
            "fish do_swim >(x)> \
            \<( \
            \swim \
            \    >(bind_this <(x)<)> \
            \    >(trout >( to_string >(bind_this)>)>)> \
            \    <(+ >(1)> >(bind_this)>)< \
            \)<"
        ),
      timedAssertEqual
        2
        "Parse a function with a swim block that contains a fishbind with a lamprey."
        []
        [ treeKeyword Return Fish
            -<= [ treeId Return "do_swim"
                    -<= [ treeId Send "x"
                            -<= [ treeKeyword Return Swim
                                    -<= [ treeId Send "bind_this"
                                            -<= [ treeKeyword Return Lamprey
                                                    -<= [ treeId Send "z",
                                                          treeId Return "z"
                                                        ]
                                                ],
                                          treeId Send "trout"
                                            -<= [ treeId Send "to_string"
                                                    -<= [ treeId Send "bind_this"
                                                            -<= [treeId Send "x"]
                                                        ]
                                                ],
                                          treeId Return "+"
                                            -<= [ (treeSU Send . dataNum) 1,
                                                  treeId Send "bind_this"
                                                    -<= [treeId Send "x"]
                                                ]
                                        ]
                                ]
                        ]
                ]
        ]
        ( prepareParser
            (functionDefinitionParser Return)
            "fish do_swim >(x)> \
            \<( \
            \swim \
            \    >(bind_this <(lamprey >(z)> <(z)<)<)> \
            \    >(trout >( to_string >(bind_this >(x)>)>)>)> \
            \    <(+ >(1)> >(bind_this >(x)>)>)< \
            \)<"
        ),
      timedAssertEqual
        2
        "Parse a function definition with multiple nested swim or function blocks."
        []
        [ treeKeyword Return Fish
            -<= [ treeId Return "top"
                    -<= [ treeKeyword Return Lamprey
                            -<= [ treeId Send "x",
                                  treeId Send "y",
                                  treeKeyword Send Fish
                                    --I'm not really sure what scopetype this is supposed
                                    --to have.
                                    -<= [ treeId Return "middle"
                                            -<= [ treeKeyword Return Lamprey
                                                    -<= [ treeId Send "z",
                                                          treeKeyword Return Swim
                                                            -<= [ treeId
                                                                    Send
                                                                    "bind_middle"
                                                                    -<= [ ( treeSU Return
                                                                              . Data
                                                                              . Boolean
                                                                          )
                                                                            True
                                                                        ],
                                                                  treeKeyword
                                                                    Return
                                                                    Lamprey
                                                                    -<= [ treeId Send "h",
                                                                          ( treeSU Return
                                                                              . Data
                                                                              . Boolean
                                                                          )
                                                                            False
                                                                        ]
                                                                ]
                                                        ]
                                                ]
                                        ],
                                  treeKeyword Return Swim
                                    -<= [ treeId Send "trout"
                                            -<= [ ( treeSU Send
                                                      . Data
                                                      . String
                                                  )
                                                    "\"Hello\""
                                                ],
                                          treeId Send "bind_top"
                                            -<= [ treeId Return "middle"
                                                    -<= [ ( treeSU Send
                                                              . Data
                                                              . Boolean
                                                          )
                                                            False
                                                        ]
                                                ],
                                          treeKeyword Return Lamprey
                                            -<= [ treeId Send "q",
                                                  treeId Return "or"
                                                    -<= [ treeId Send "q",
                                                          treeId Send "bind_top"
                                                        ]
                                                ]
                                        ]
                                ]
                        ]
                ]
        ]
        ( prepareParser
            (functionDefinitionParser Return)
            "fish top \
            \ >(x)> >(y)> \
            \ >(fish middle >(z)> <( \
            \ swim \
            \   >(bind_middle <(True)< )> \
            \   <(lamprey >(h)> <(False)< )< )< )> \
            \ <( \
            \   swim \
            \     >(trout >(\"Hello\")>)> \
            \     >(bind_top <(middle >(False)>)<)> \
            \     <( >(q)> <( or >(q)> >(bind_top)> )< )< \
            \ )<"
        )
    ]
