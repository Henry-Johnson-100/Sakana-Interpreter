import ExecutionTree
import Lexer
import SyntaxTree
import Test.Tasty
  ( Timeout (Timeout),
    defaultMain,
    localOption,
    testGroup,
  )
import Test.Tasty.HUnit (assertEqual, testCase)
import Token.Data
import Token.Util.Tree

executeFirstChild :: String -> Data
executeFirstChild = ExecutionTree.evaluateNode . ExecutionTree.calct'

-- | main
main = do
  defaultMain tests

tests = testGroup "ExecutionTree tests" testList
  where
    testList =
      concat
        [ numOperatorTests,
          boolOperatorTests
        ]

bulkTest name d assertionsAndFunctions =
  map
    (\(assert, func) -> testCase name (assertEqual d assert func))
    assertionsAndFunctions

standardTimeout timeS = localOption (Timeout (timeS * 1000000) (show timeS ++ "s"))

numOperatorTests =
  bulkTest
    "Simple Num operator tests"
    "Using fish operators"
    ( zip
        ( map
            Num
            [ 0.0,
              1.0,
              2.0,
              6.0,
              10.0,
              100.0,
              25.0
            ]
        )
        ( map
            executeFirstChild
            [ "+ >(0)> >(0)>",
              "+ >(0)> >(1)>",
              "+ >(1)> >(1)>",
              "* >(2)> >(3)>",
              "+ >(2)> >(* >(2)> >(4)>)>",
              "^ >(10)> >(2)>",
              "+ >(^ >(3)> >(2)>)> >(^ >(* >(2)> >(2)>)> >(2)>)>"
            ]
        )
    )

boolOperatorTests =
  bulkTest
    "Boolean expression tests"
    "Using fish operators"
    ( zip
        ( map
            Boolean
            [ True,
              True,
              True,
              True,
              False,
              False,
              False,
              False
            ]
        )
        ( map
            executeFirstChild
            [ "== >(0)> >(0)>",
              "> >(5)> >(0)>",
              ">= >(5)> >(5)>",
              "/= >(4.999)> >(5)>",
              "== >(True)> >(False)>",
              "/= >(1)> >(1)>",
              "< >(100)> >(0)>",
              ">= >(4.999)> >(5)>"
            ]
        )
    )

-- #TODO
-- bindingRecallTests = testGroup "value binding recall tests" testList
--   where
--     testList =
--       []

-- canRecallSimpleGlobalBinding = testCase name assertion where
--   name = "Recall primitive global value binding"
--   assertion = assertEqual d a f
--   d = "Recal primitive global value binding"
--   a = Num 1.0
--   f = 