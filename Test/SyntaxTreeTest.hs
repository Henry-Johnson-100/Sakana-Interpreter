import Lexer
import SyntaxTree
import Test.Tasty
import Test.Tasty.HUnit
import Token.Bracket
import Token.Control
import Token.Data
import Token.Keyword
import Token.Operator
import Util.General
import Util.Tree

standardTimeout timeS = localOption (Timeout (timeS * 1000000) (show timeS ++ "s"))

prepareString = generateSyntaxTree . tokenize

dataId = Data . Id

dataNum = Data . Num

makeSU c t = SyntaxUnit t 1 c

treeSU c t = tree . makeSU c $ t

attachToMain trs = (tree . genericSyntaxUnit) (Data (Id "main")) -<= trs

main = do
  defaultMain tests

tests = testGroup "SyntaxTree Tests" testList
  where
    testList =
      [treeGenerationTests]

treeGenerationTests = testGroup "SyntaxTree generation tests" testList
  where
    testList =
      [ treeSimpleFunctionOne,
        treeSimpleFunctionTwo,
        treeSimpleFunctionThree,
        treeMultipleFunctionsOne,
        treeMultipleFunctionsTwo,
        treeMultipleFunctionsThree,
        treeSubFunctionsOne
      ]

treeSimpleFunctionOne = testCase name assertion
  where
    name = "A tree is properly generated for a simple function (1)."
    assertion = assertEqual name a f
    a =
      attachToMain
        [ (tree . makeSU Return . Keyword) Fish
            -<= [(treeSU Return . dataId) "simple", (treeSU Send . dataId) "x", (treeSU Return . dataId) "x"]
        ]
    f = prepareString "fish simple >(x)> <(x)<"

treeSimpleFunctionTwo = testCase name assertion
  where
    name = "A tree is properly generated for a simple function (2)."
    assertion = assertEqual name a f
    a =
      attachToMain
        [ (tree . makeSU Return . Keyword) Fish
            -<= [ (treeSU Return . dataId) "simple",
                  (treeSU Send . dataId) "x",
                  (treeSU Return . Operator) Add
                    -<= [(treeSU Send . dataId) "x", (treeSU Send . dataNum) 1.0]
                ]
        ]
    f = prepareString "fish simple >(x)> <(+ >(x)> >(1)>)<"

treeSimpleFunctionThree = testCase name assertion
  where
    name = "A tree is properly generated for a simple function (3)."
    assertion = assertEqual name a f
    a =
      attachToMain
        [ (tree . makeSU Return . Keyword) Fish
            -<= [ (treeSU Return . dataId) "simple",
                  (treeSU Send . dataId) "x",
                  (treeSU Return . Operator) Add
                    -<= [ (treeSU Send . dataId) "x",
                          (treeSU Send . Operator) Add
                            -<= [(treeSU Send . dataNum) 1.0, (treeSU Send . dataNum) 1.0]
                        ]
                ]
        ]
    f = prepareString "fish simple >(x)> <(+ >(x)> >(+ >(1)> >(1)>)>)<"

treeMultipleFunctionsOne = testCase name assertion
  where
    name = "A tree is properly generated for multiple simple function (1)."
    assertion = assertEqual name a f
    a =
      attachToMain
        [ (tree . makeSU Return . Keyword) Fish
            -<= [ (treeSU Return . dataId) "simple",
                  (treeSU Send . dataId) "x",
                  (treeSU Return . Operator) Add
                    -<= [ (treeSU Send . dataId) "x",
                          (treeSU Send . Operator) Add
                            -<= [(treeSU Send . dataNum) 1.0, (treeSU Send . dataNum) 1.0]
                        ]
                ],
          (treeSU Return . Keyword) Fish -- This is the current error
            -<= [ (treeSU Return . dataId) "two",
                  (treeSU Send . dataId) "x",
                  (treeSU Return . dataId) "x"
                ]
        ]
    f = prepareString "fish simple >(x)> <(+ >(x)> >(+ >(1)> >(1)>)>)< fish two >(x)> <(x)<"

treeMultipleFunctionsTwo = testCase name assertion
  where
    name = "A tree is properly generated for multiple simple function (2)."
    assertion = assertEqual name a f
    a =
      attachToMain
        [ (tree . makeSU Return . Keyword) Fish
            -<= [ (treeSU Return . dataId) "simple",
                  (treeSU Send . dataId) "x",
                  (treeSU Return . Operator) Add
                    -<= [ (treeSU Send . dataId) "x",
                          (treeSU Send . Operator) Add
                            -<= [(treeSU Send . dataNum) 1.0, (treeSU Send . dataNum) 1.0]
                        ]
                ],
          (treeSU Return . Keyword) Fish -- This is the current error
            -<= [ (treeSU Return . dataId) "two",
                  (treeSU Send . dataId) "x",
                  (treeSU Return . Operator) Add
                    -<= [ (treeSU Send . dataId) "x",
                          (treeSU Send . Operator) Add
                            -<= [(treeSU Send . dataNum) 1.0, (treeSU Send . dataNum) 1.0]
                        ]
                ]
        ]
    f = prepareString "fish simple >(x)> <(+ >(x)> >(+ >(1)> >(1)>)>)< fish two >(x)> <(+ >(x)> >(+ >(1)> >(1)>)>)<"

treeMultipleFunctionsThree = testCase name assertion
  where
    name = "A tree is properly generated for multiple simple function (3)."
    assertion = assertEqual name a f
    a =
      attachToMain
        [ (tree . makeSU Return . Keyword) Fish
            -<= [ (treeSU Return . dataId) "simple",
                  (treeSU Send . dataId) "x",
                  (treeSU Return . Operator) Add
                    -<= [ (treeSU Send . dataId) "x",
                          (treeSU Send . Operator) Add
                            -<= [(treeSU Send . dataNum) 1.0, (treeSU Send . dataNum) 1.0]
                        ]
                ],
          (treeSU Return . Keyword) Fish -- This is the current error
            -<= [ (treeSU Return . dataId) "two",
                  (treeSU Send . dataId) "x",
                  (treeSU Return . Operator) Add
                    -<= [ (treeSU Send . dataId) "x",
                          (treeSU Send . Operator) Add
                            -<= [(treeSU Send . dataNum) 1.0, (treeSU Send . dataNum) 1.0]
                        ]
                ],
          (treeSU Return . Keyword) Fish -- This is the current error
            -<= [ (treeSU Return . dataId) "two",
                  (treeSU Send . dataId) "x",
                  (treeSU Return . Operator) Add
                    -<= [ (treeSU Send . dataId) "x",
                          (treeSU Send . Operator) Add
                            -<= [(treeSU Send . dataNum) 1.0, (treeSU Send . dataNum) 1.0]
                        ]
                ]
        ]
    f =
      prepareString
        "fish simple >(x)> <(+ >(x)> >(+ >(1)> >(1)>)>)< fish two >(x)> <(+ >(x)> >(+ >(1)> >(1)>)>)<\
        \fish three >(x)> <(+ >(x)> >(+ >(1)> >(1)>)>)<"

treeSubFunctionsOne = testCase name assertion
  where
    name = "A tree is properly generated for functions with subfunctions (1)."
    assertion = assertEqual name a f
    a =
      attachToMain
        [ (treeSU Return . Keyword) Fish
            -<= [ (treeSU Return . dataId) "fact",
                  (treeSU Send . dataId) "n",
                  (treeSU Send . Keyword) Fish
                    -<= [ (treeSU Send . dataId) "sub_fact",
                          (treeSU Send . dataId) "sub",
                          (treeSU Send . dataId) "prd",
                          (treeSU Return . Control) Fin
                            -<= [ (treeSU Send . Operator) LtEq
                                    -<= [ (treeSU Send . dataId) "sub",
                                          (treeSU Send . dataNum) 0.0
                                        ],
                                  (treeSU Send . dataId) "prd",
                                  (treeSU Send . dataId) "sub_fact"
                                    -<= [ (treeSU Send . Operator) Sub
                                            -<= [ (treeSU Send . dataId) "sub",
                                                  (treeSU Send . dataNum) 1.0
                                                ],
                                          (treeSU Send . Operator) Mult
                                            -<= [ (treeSU Send . dataId) "sub",
                                                  (treeSU Send . dataId) "prd"
                                                ]
                                        ]
                                ]
                        ],
                  (treeSU Return . dataId) "sub_fact"
                    -<= [ (treeSU Send . dataNum) 30.0,
                          (treeSU Send . dataNum) 1.0
                        ]
                ]
        ]
    f =
      prepareString
        "fish fact >(n)> >(fish sub_fact >(sub)> >(prd)> <(\
        \fin >(<= >(sub)> >(0)>)>\
        \>(prd)>\
        \>(sub_fact >(- >(sub)> >(1)>)> >(* >(sub)> >(prd)>)>)>\
        \)<)>\
        \<(sub_fact >(30)> >(1)>)<"