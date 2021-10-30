import SakanaParser
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

prepareString = generateSyntaxTree

dataId = Data . Id

dataNum = Data . Num

dataString = Data . String

makeSU c t = SyntaxUnit t 1 c

treeSU c t = (tree . makeSU c) t

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
        treeSubFunctionsOne,
        treeSubFunctionsTwo,
        treeFunctionWithSwimExecutionOne,
        treeFunctionWithSwimExecutionTwo,
        commentsAreIgnoredOne
      ]

{-
  What I expect from a function tree ->
    The declaration keyword (Fish.. etc) is the function's parent node.
    Everything else in a function is a child of that node.
    All function declarations in a top-level scope (main, in a school, in a shoal ..etc)
      -> should have a context of Return
      -> Otherwise, they will have a context of whatever kind of fish they appear in.
    The id of the declaration is the very first child, and always has a context of Send
      -> This context does not actually matter for execution,
          but should be Send for posterity and consistency.
    Following the id, the arguments and supplemental information.
    Then the return block which is one node,
      with a Return context, and some arbitrary number of children.

  Using a swim keyword ->
    It should not be necessary to wrap a swim block in a return fish.
    The fact that this throws an error if not done is a problem with tree generation.
    Specifically, the fact that values between fish are ignored or thrown out,
      and that extra checks have to be done before ignoring them.
    Generating a tree should be able to be done recursively with ease instead of the
      less-than-desirable TriplePartition method I am using right now.
    Swim should ALWAYS have a Return context.
-}

treeSimpleFunctionOne = testCase name assertion
  where
    name = "A tree is properly generated for a simple function (1)."
    assertion = assertEqual name a f
    a =
      attachToMain
        [ (tree . makeSU Return . Keyword) Fish
            -<= [(treeSU Send . dataId) "simple", (treeSU Send . dataId) "x", (treeSU Return . dataId) "x"]
        ]
    f = prepareString "fish simple >(x)> <(x)<"

treeSimpleFunctionTwo = testCase name assertion
  where
    name = "A tree is properly generated for a simple function (2)."
    assertion = assertEqual name a f
    a =
      attachToMain
        [ (tree . makeSU Return . Keyword) Fish
            -<= [ (treeSU Send . dataId) "simple",
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
            -<= [ (treeSU Send . dataId) "simple",
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
            -<= [ (treeSU Send . dataId) "simple",
                  (treeSU Send . dataId) "x",
                  (treeSU Return . Operator) Add
                    -<= [ (treeSU Send . dataId) "x",
                          (treeSU Send . Operator) Add
                            -<= [(treeSU Send . dataNum) 1.0, (treeSU Send . dataNum) 1.0]
                        ]
                ],
          (treeSU Return . Keyword) Fish -- This is the current error
            -<= [ (treeSU Send . dataId) "two",
                  (treeSU Send . dataId) "x",
                  (treeSU Return . dataId) "x"
                ]
        ]
    f =
      prepareString
        "fish simple >(x)> <(+ >(x)> >(+ >(1)> >(1)>)>)<\
        \ fish two >(x)> <(x)<"

treeMultipleFunctionsTwo = testCase name assertion
  where
    name = "A tree is properly generated for multiple simple function (2)."
    assertion = assertEqual name a f
    a =
      attachToMain
        [ (tree . makeSU Return . Keyword) Fish
            -<= [ (treeSU Send . dataId) "simple",
                  (treeSU Send . dataId) "x",
                  (treeSU Return . Operator) Add
                    -<= [ (treeSU Send . dataId) "x",
                          (treeSU Send . Operator) Add
                            -<= [(treeSU Send . dataNum) 1.0, (treeSU Send . dataNum) 1.0]
                        ]
                ],
          (treeSU Return . Keyword) Fish -- This is the current error
            -<= [ (treeSU Send . dataId) "two",
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
        "fish simple >(x)> <(+ >(x)> >(+ >(1)> >(1)>)>)< fish two >(x)>\
        \<(+ >(x)> >(+ >(1)> >(1)>)>)<"

treeMultipleFunctionsThree = testCase name assertion
  where
    name = "A tree is properly generated for multiple simple function (3)."
    assertion = assertEqual name a f
    a =
      attachToMain
        [ (tree . makeSU Return . Keyword) Fish
            -<= [ (treeSU Send . dataId) "simple",
                  (treeSU Send . dataId) "x",
                  (treeSU Return . Operator) Add
                    -<= [ (treeSU Send . dataId) "x",
                          (treeSU Send . Operator) Add
                            -<= [(treeSU Send . dataNum) 1.0, (treeSU Send . dataNum) 1.0]
                        ]
                ],
          (treeSU Return . Keyword) Fish -- This is the current error
            -<= [ (treeSU Send . dataId) "two",
                  (treeSU Send . dataId) "x",
                  (treeSU Return . Operator) Add
                    -<= [ (treeSU Send . dataId) "x",
                          (treeSU Send . Operator) Add
                            -<= [(treeSU Send . dataNum) 1.0, (treeSU Send . dataNum) 1.0]
                        ]
                ],
          (treeSU Return . Keyword) Fish -- This is the current error
            -<= [ (treeSU Send . dataId) "three",
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
        "fish simple >(x)> <(+ >(x)> >(+ >(1)> >(1)>)>)<\
        \fish two >(x)> <(+ >(x)> >(+ >(1)> >(1)>)>)<\
        \fish three >(x)> <(+ >(x)> >(+ >(1)> >(1)>)>)<"

treeSubFunctionsOne = testCase name assertion
  where
    name = "A tree is properly generated for functions with subfunctions (1)."
    assertion = assertEqual name a f
    a =
      attachToMain
        [ (treeSU Return . Keyword) Fish
            -<= [ (treeSU Send . dataId) "fact",
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

treeSubFunctionsTwo :: TestTree
treeSubFunctionsTwo = testCase name assertion
  where
    name = "A tree is properly generated for functions with subfunctions (2)."
    assertion = assertEqual name a f
    a =
      attachToMain
        [ (treeSU Return . Keyword) Fish
            -<= [ (treeSU Send . dataId) "fact",
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
                ],
          (treeSU Return . Keyword) Fish -- This will probably be where it fails
            -<= [ (treeSU Send . dataId) "fact_",
                  (treeSU Send . dataId) "n_",
                  (treeSU Send . Keyword) Fish
                    -<= [ (treeSU Send . dataId) "sub_fact_",
                          (treeSU Send . dataId) "sub_",
                          (treeSU Send . dataId) "prd_",
                          (treeSU Return . Control) Fin
                            -<= [ (treeSU Send . Operator) LtEq
                                    -<= [ (treeSU Send . dataId) "sub_",
                                          (treeSU Send . dataNum) 0.0
                                        ],
                                  (treeSU Send . dataId) "prd_",
                                  (treeSU Send . dataId) "sub_fact_"
                                    -<= [ (treeSU Send . Operator) Sub
                                            -<= [ (treeSU Send . dataId) "sub_",
                                                  (treeSU Send . dataNum) 1.0
                                                ],
                                          (treeSU Send . Operator) Mult
                                            -<= [ (treeSU Send . dataId) "sub_",
                                                  (treeSU Send . dataId) "prd_"
                                                ]
                                        ]
                                ]
                        ],
                  (treeSU Return . dataId) "sub_fact_"
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
        \<(sub_fact >(30)> >(1)>)< \
        \fish fact_ >(n_)> >(fish sub_fact_ >(sub_)> >(prd_)> <(\
        \fin >(<= >(sub_)> >(0)>)>\
        \>(prd_)>\
        \>(sub_fact_ >(- >(sub_)> >(1)>)> >(* >(sub_)> >(prd_)>)>)>\
        \)<)>\
        \<(sub_fact_ >(30)> >(1)>)<"

treeFunctionWithSwimExecutionOne :: TestTree
treeFunctionWithSwimExecutionOne = testCase name assertion
  where
    name = "A function with a swim execution block(1)."
    assertion = assertEqual name a f
    a =
      attachToMain
        [ (treeSU Return . Keyword) Fish
            -<= [ (treeSU Send . dataId) "fact",
                  (treeSU Send . dataId) "n",
                  (treeSU Send . Keyword) Fish
                    -<= [ (treeSU Send . dataId) "sub_fact",
                          (treeSU Send . dataId) "sub",
                          (treeSU Send . dataId) "prd",
                          (treeSU Return . Keyword) Swim --This will throw an error now
                            -<= [ (treeSU Send . dataId) "trout"
                                    -<= [(treeSU Send . dataString) "Printing something"],
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
        "fish fact >(n)> >(fish sub_fact >(sub)> >(prd)> \
        \swim\
        \>(trout >(\"Printing something\")>)>\
        \<(\
        \fin >(<= >(sub)> >(0)>)>\
        \>(prd)>\
        \>(sub_fact >(- >(sub)> >(1)>)> >(* >(sub)> >(prd)>)>)>\
        \)<\
        \)>\
        \<(sub_fact >(30)> >(1)>)<"

treeFunctionWithSwimExecutionTwo :: TestTree
treeFunctionWithSwimExecutionTwo = testCase name assertion
  where
    name = "A function with a swim execution block(2)."
    assertion = assertEqual name a f
    a =
      attachToMain
        [ (treeSU Return . Keyword) Fish
            -<= [ (treeSU Send . dataId) "fact",
                  (treeSU Send . dataId) "n",
                  (treeSU Send . Keyword) Fish
                    -<= [ (treeSU Send . dataId) "sub_fact",
                          (treeSU Send . dataId) "sub",
                          (treeSU Send . dataId) "prd",
                          (treeSU Return . Keyword) Swim --This will throw an error now
                            -<= [ (treeSU Send . dataId) "trout"
                                    -<= [(treeSU Send . dataString) "Printing something"],
                                  (treeSU Return . Control) Fin
                                    -<= [ (treeSU Send . Operator) LtEq
                                            -<= [ (treeSU Send . dataId) "sub",
                                                  (treeSU Send . dataNum) 0.0
                                                ],
                                          (treeSU Return . Keyword) Swim
                                            -<= [ (treeSU Send . dataId) "trout"
                                                    -<= [(treeSU Send . dataString) "prd"],
                                                  (treeSU Return . dataId) "prd"
                                                ],
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
        "fish fact >(n)> >(fish sub_fact >(sub)> >(prd)> \
        \swim\
        \>(trout >(\"Printing something\")>)>\
        \<(\
        \fin >(<= >(sub)> >(0)>)>\
        \>(\
        \swim\
        \>(trout >(\"prd\")>)>\
        \<(prd)<\
        \)>\
        \>(sub_fact >(- >(sub)> >(1)>)> >(* >(sub)> >(prd)>)>)>\
        \)<\
        \)>\
        \<(sub_fact >(30)> >(1)>)<"

commentsAreIgnoredOne = testCase name assertion
  where
    name = "Block comments are ignored (1)."
    assertion = assertEqual name a f
    a =
      attachToMain
        [ (tree . makeSU Return . Keyword) Fish
            -<= [(treeSU Send . dataId) "simple", (treeSU Send . dataId) "x", (treeSU Return . dataId) "x"]
        ]
    f = prepareString "/* hi */ fish simple >(x)> <(x)<"