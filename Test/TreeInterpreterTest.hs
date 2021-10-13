import Data.Maybe
import ExecutionTree
import Lexer
import SyntaxTree
import Test.Tasty
import Test.Tasty.HUnit
import Token.Data
import Token.Util.Tree

executeFirstChild :: String -> Data
executeFirstChild = ExecutionTree.evaluateNode noEnv . ExecutionTree.calct'

getMainTree :: String -> SyntaxTree
getMainTree = generateSyntaxTree . tokenize

agnosticizeLines tr = fmap (setLineToZero) tr
  where
    setLineToZero (SyntaxUnit t _ c) = SyntaxUnit t 0 c

noLineCalct = agnosticizeLines . calct'

-- | main
main = do
  defaultMain tests

tests = testGroup "ExecutionTree tests" testList
  where
    testList =
      [ finTests,
        functionDisambiguationTests,
        functionDeclArgFetchingTests,
        functionExecutionTests
      ]
        ++ concat
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

finTests = testGroup "Fin control tests" testList
  where
    testList =
      [
        finWorksOnExplicitBool,
        finWorksOnImplicitBool
      ]

finWorksOnExplicitBool = testCase name assertion
  where
    name = "Fin with explicit bool first arg works"
    assertion = assertEqual d a f
    d = name
    a = Num 1.0
    f = executeMain . getMainTree $ "fin >(True)> >(1)> >(0)>"

finWorksOnImplicitBool = testCase name assertion
  where
    name = "Fin with implicit bool first arg works"
    assertion = assertEqual d a f
    d = name
    a = Num 0.0
    f = executeMain . getMainTree $ "fin >(500)> >(1)> >(0)>"

nestedFinWorks = testCase name assertion
  where
    name = "nested fins work"
    assertion = assertEqual d a f
    d = name
    a = Num 0.0
    f = executeMain . getMainTree $ "fin >(500)> >(fin >(True)> >(1)> >(0)>)> >(fin >(True)> >(0)> >(1)>)>"

functionDeclArgFetchingTests = testGroup "Fetching arguments from function declarations" testList
  where
    testList =
      [ oneFuncArgIsFetched,
        twoFuncArgIsFetched,
        threeFuncArgIsFetched,
        allPosArgsAreFetchedOne,
        allPosArgsAreFetchedTwo,
        allPosArgsAreFetchedThree,
        allPosArgsAreFetchedMultipleMixesOne,
        allPosArgsAreFetchedMultipleMixesOne
      ]

oneFuncArgIsFetched = testCase name assertion
  where
    name = "A function declaration with one positional arg(s) is fetched appropriately"
    assertion = assertEqual d a f
    d = name
    a = [noLineCalct] <*> [">(n)>"]
    f = getFuncDeclArgs . noLineCalct $ "fish return_n >(n)> <(n)<"

twoFuncArgIsFetched = testCase name assertion
  where
    name = "A function declaration with two positional arg(s) is fetched appropriately"
    assertion = assertEqual d a f
    d = name
    a = [noLineCalct] <*> [">(n)>", ">(m)>"]
    f = getFuncDeclArgs . noLineCalct $ "fish return_n >(n)> >(m)> <(n)<"

threeFuncArgIsFetched = testCase name assertion
  where
    name = "A function declaration with three positional arg(s) is fetched appropriately"
    assertion = assertEqual d a f
    d = name
    a = [noLineCalct] <*> [">(n)>", ">(m)>", ">(o)>"]
    f = getFuncDeclArgs . noLineCalct $ "fish return_n >(n)> >(m)> >(o)> <(n)<"

allPosArgsAreFetchedOne = testCase name assertion
  where
    name = "A function declaration with three positional arg(s) is fetched appropriately"
    assertion = assertEqual d a f
    d = name
    a = [noLineCalct] <*> [">(n)>", ">(m)>", ">(o)>"]
    f = getFunctionDeclPositionalArgs . noLineCalct $ "fish return_n >(n)> >(m)> >(o)> <(n)<"

allPosArgsAreFetchedTwo = testCase name assertion
  where
    name = "A function declaration with one positional arg(s) is fetched appropriately"
    assertion = assertEqual d a f
    d = name
    a = [noLineCalct] <*> [">(n)>"]
    f = getFunctionDeclPositionalArgs . noLineCalct $ "fish return_n >(n)> <(n)<"

allPosArgsAreFetchedThree = testCase name assertion
  where
    name = "A function declaration with mixed positional arg(s) is fetched appropriately"
    assertion = assertEqual d a f
    d = name
    a = [noLineCalct] <*> [">(n)>", ">(o)>"]
    f = getFunctionDeclPositionalArgs . noLineCalct $ "fish return_n >(n)> >(m <(1)<)> >(o)> <(n)<"

allPosArgsAreFetchedMultipleMixesOne = testCase name assertion
  where
    name = "A function declaration with mixed positional arg(s) is fetched appropriately"
    assertion = assertEqual d a f
    d = name
    a = [noLineCalct] <*> [">(n)>", ">(o)>", ">(q)>"]
    f = getFunctionDeclPositionalArgs . noLineCalct $ "fish return_n >(n)> >(m <(1)<)> >(o)> >(p <(1)<)> >(q)> <(n)<"

allPosArgsAreFetchedMultipleMixesTwo = testCase name assertion
  where
    name = "A function declaration with mixed positional arg(s) and a function declaration is fetched appropriately"
    assertion = assertEqual d a f
    d = name
    a = [noLineCalct] <*> [">(n)>", ">(o)>", ">(q)>"]
    f = getFunctionDeclPositionalArgs . noLineCalct $ "fish return_n >(n)> >(m <(1)<)> >(o)> >(fish thing >(x)> <(x)<)> >(q)> <(n)<"

functionDisambiguationTests = testGroup "Function disambiguation tests" testList
  where
    testList =
      [ simpleFunctionIsDisambiguated,
        multiplePositionalArgsAreDisambiguated,
        functionWithArgMixIsDisambiguatedOne,
        functionWithArgMixIsDisambiguatedTwo,
        functionWithArgMixIsDisambiguatedThree
      ]

simpleFunctionIsDisambiguated = testCase name assertion
  where
    name = "Simple function can be disambiguated"
    assertion = assertEqual d a f
    d = "Simple function can be disambiguated"
    a = noLineCalct "fish return_n >(n <(1)<)> <(n)<"
    f =
      disambiguateFunction
        (noLineCalct "return_n >(1)>")
        (noLineCalct "fish return_n >(n)> <(n)<")

multiplePositionalArgsAreDisambiguated = testCase name assertion
  where
    name = "function declaration with multiple pos. args is disambiguated"
    assertion = assertEqual d a f
    d = name
    a = noLineCalct "fish add >(x <(1)<)> >(y <(2)<)> <(something)<"
    f =
      disambiguateFunction
        (noLineCalct "add >(1)> >(2)>")
        (noLineCalct "fish add >(x)> >(y)> <(something)<")

functionWithArgMixIsDisambiguatedOne = testCase name assertion
  where
    name = "A function with a mix of positional args and sending data is disambiguated"
    assertion = assertEqual d a f
    d = name
    a = noLineCalct "fish xyz >(x <(1)<)> >(y <(2)<)> >(z <(3)<)> <(z)<"
    f = disambiguateFunction (noLineCalct "xyz >(1)> >(2)>") (noLineCalct "fish xyz >(x)> >(y)> >(z <(3)<)> <(z)<")

functionWithArgMixIsDisambiguatedTwo = testCase name assertion
  where
    name = "A function with a mix of positional args and sending data is disambiguated"
    assertion = assertEqual d a f
    d = name
    a = noLineCalct "fish xyz >(x <(1)<)> >(y <(2)<)> >(z <(3)<)> <(z)<"
    f = disambiguateFunction (noLineCalct "xyz >(1)> >(3)>") (noLineCalct "fish xyz >(x)> >(y <(2)<)> >(z)> <(z)<")

functionWithArgMixIsDisambiguatedThree = testCase name assertion
  where
    name = "A function with a mix of positional args and sending data is disambiguated"
    assertion = assertEqual d a f
    d = name
    a = noLineCalct "fish xyz >(x <(1)<)> >(y <(2)<)> >(z <(3)<)> <(z)<"
    f = disambiguateFunction (noLineCalct "xyz >(2)> >(3)>") (noLineCalct "fish xyz >(x <(1)<)> >(y)> >(z)> <(z)<")

functionWithMultipleArgMixIsDisambiguatedOne = testCase name assertion
  where
    name = "A function with a mix of positional args and sending data is disambiguated"
    assertion = assertEqual d a f
    d = name
    a = noLineCalct "fish xyz >(x <(1)<)> >(y <(2)<)> >(z <(3)<)> >(a <(4)<)> >(b <(5)<)> <(z)<"
    f = disambiguateFunction (noLineCalct "xyz >(1)> >(3)> >(5)>") (noLineCalct "fish xyz >(x)> >(y <(2)<)> >(z)> >(a <(4)<)> >(b)> <(z)<")


functionExecutionTests = testGroup "Function execution tests" testList 
  where
    testList =
      [
        executesVerySimpleFunction,
        executesVerySimpleFunctionTwo,
        executesMixedArgFunction,
        executesFunctionWithFin,
        executesFunctionWithSubFunc
      ]

executesVerySimpleFunction = testCase name assertion
  where
    name = "A very simple function can be defined and executed"
    assertion = assertEqual d a f
    d = name
    a = Num 1.0
    f = executeMain . getMainTree $ "fish return_n >(n)> <(n)< <(return_n >(1)>)<"

executesVerySimpleFunctionTwo = testCase name assertion
  where
    name = "A very simple function can be defined and executed"
    assertion = assertEqual d a f
    d = name
    a = Num 2.0
    f = executeMain . getMainTree $ "fish add >(x)> >(y)> <(+ >(x)> >(y)>)< <(add >(1)> >(1)>)<"

executesMixedArgFunction = testCase name assertion
  where
    name = "A very simple function can be defined and executed"
    assertion = assertEqual d a f
    d = name
    a = Num 2.0
    f = executeMain . getMainTree $ "fish add >(x)> >(y <(1)<)> <(+ >(x)> >(y)>)< <(add >(1)>)<"

executesFunctionWithFin = testCase name assertion
  where
    name = "A function with a fin call can be executed"
    assertion = assertEqual d a f
    d = name
    a = Num 2.0
    f = executeMain . getMainTree $ "fish to_bool >(x)> <(fin >(x)> >(True)> >(False)>)< <(to_bool >(1)>)<"

executesFunctionWithSubFunc = testCase name assertion
  where
    name = "A function with a sub-function definition can be executed"
    assertion = assertEqual d a f
    d = name
    a = Num 0.0
    f = executeMain . getMainTree $ "fish subtract_one >(x)> >(fish sub >(y)> <(- >(y)> >(1)>)<)> <(sub >(x)>)< <(subtract_one >(1)>)<"