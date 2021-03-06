import Data.Maybe
-- import Lexer
-- import SyntaxTree

import SakanaParser
import System.IO.Unsafe
import Test.Tasty
import Test.Tasty.HUnit
import Token.Bracket
import Token.Data
import TreeInterpreter
import TreeInterpreter.Environment
import Util.General
import Util.Tree

executeFirstChild :: String -> Data
executeFirstChild =
  unsafePerformIO
    . TreeInterpreter.execute
    . runtimeUnit
    . fromJust
    . Util.General.head'
    . treeChildren
    . generateSyntaxTree

getMainTree :: String -> IO SyntaxTree
getMainTree = return . generateSyntaxTree

prepareFunctionForTest :: [Char] -> Data
prepareFunctionForTest str = unsafePerformIO $ executeMain (exRT str) (return "")
  where
    docTree = SakanaParser.generateSyntaxTree
    exRT = getMainRuntime . docTree

agnosticizeLines tr = fmap (setLineToZero) tr
  where
    setLineToZero (SyntaxUnit t _ c) = SyntaxUnit t 0 c

noLineCalct = agnosticizeLines . fromJust . Util.General.head' . treeChildren . generateSyntaxTree

fishEnv =
  "fish to_bool\
  \ >(x)>\
  \  <(\
  \  fin\
  \  >(x)>\
  \  >(True)>\
  \  >(False)>\
  \ )<\
  \fish and\
  \ >(x)>\
  \ >(y)>\
  \  <(\
  \   fin\
  \  >(x)>\
  \ >(to_bool >(y)>)>\
  \ >(False)>\
  \)<\
  \fish not\
  \ >(x)>\
  \ <(\
  \ fin\
  \ >(to_bool >(x)>)>\
  \  >(False)>\
  \  >(True)>\
  \ )<\
  \fish or\
  \ >(x)>\
  \  >(y)>\
  \<(\
  \  fin\
  \ >(x)>\
  \ >(True)>\
  \  >(to_bool >(y)>)>\
  \ )<\
  \fish incr\
  \>(n)>\
  \<(+ >(n)> >(1)>)<"

fishCall :: [Char] -> [Char]
fishCall str = (fishEnv ++ " swim ") ++ ("<(" ++ str ++ ")<")

-- | main
main = do
  defaultMain tests

tests = testGroup "TreeInterpreter tests" testList
  where
    testList =
      [ finTests,
        -- functionDisambiguationTests,
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
            (Num)
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
            (Boolean)
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
      [ finWorksOnExplicitBool,
        finWorksOnImplicitBool
      ]

finWorksOnExplicitBool = standardTimeout 3 $ testCase name assertion
  where
    name = "Fin with explicit bool first arg works"
    assertion = assertEqual d a f
    d = name
    a = Num 1.0
    f = prepareFunctionForTest $ "fin >(True)> >(1)> >(0)>"

finWorksOnImplicitBool = standardTimeout 3 $ testCase name assertion
  where
    name = "Fin with implicit bool first arg works"
    assertion = assertEqual d a f
    d = name
    a = Num 0.0
    f = prepareFunctionForTest $ "fin >(500)> >(1)> >(0)>"

nestedFinWorks = standardTimeout 3 $ testCase name assertion
  where
    name = "nested fins work"
    assertion = assertEqual d a f
    d = name
    a = Num 0.0
    f = prepareFunctionForTest $ "fin >(500)> >(fin >(True)> >(1)> >(0)>)> >(fin >(True)> >(0)> >(1)>)>"

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

assertArgs xs = fmap agnosticizeLines $ map (tree . rotateArg3 SyntaxUnit 0 Send . Data . Id) xs

oneFuncArgIsFetched = standardTimeout 3 $ testCase name assertion
  where
    name = "A function declaration with one positional arg(s) is fetched appropriately"
    assertion = assertEqual d a f
    d = name
    a = assertArgs ["n"]
    f = getFuncDeclArgs . noLineCalct $ "fish return_n >(n)> <(n)<"

twoFuncArgIsFetched = standardTimeout 3 $ testCase name assertion
  where
    name = "A function declaration with two positional arg(s) is fetched appropriately"
    assertion = assertEqual d a f
    d = name
    a = assertArgs ["n", "m"]
    f = getFuncDeclArgs . noLineCalct $ "fish return_n >(n)> >(m)> <(n)<"

threeFuncArgIsFetched = standardTimeout 3 $ testCase name assertion
  where
    name = "A function declaration with three positional arg(s) is fetched appropriately"
    assertion = assertEqual d a f
    d = name
    a = assertArgs ["n", "m", "o"]
    f = getFuncDeclArgs . noLineCalct $ "fish return_n >(n)> >(m)> >(o)> <(n)<"

allPosArgsAreFetchedOne = standardTimeout 3 $ testCase name assertion
  where
    name = "A function declaration with three positional arg(s) is fetched appropriately"
    assertion = assertEqual d a f
    d = name
    a = assertArgs ["n", "m", "o"]
    f = getFunctionDeclPositionalArgs . noLineCalct $ "fish return_n >(n)> >(m)> >(o)> <(n)<"

allPosArgsAreFetchedTwo = standardTimeout 3 $ testCase name assertion
  where
    name = "A function declaration with one positional arg(s) is fetched appropriately"
    assertion = assertEqual d a f
    d = name
    a = assertArgs ["n"]
    f = getFunctionDeclPositionalArgs . noLineCalct $ "fish return_n >(n)> <(n)<"

allPosArgsAreFetchedThree = standardTimeout 3 $ testCase name assertion
  where
    name = "A function declaration with mixed positional arg(s) is fetched appropriately"
    assertion = assertEqual d a f
    d = name
    a = assertArgs ["n", "o"]
    f = getFunctionDeclPositionalArgs . noLineCalct $ "fish return_n >(n)> >(fish m <(1)<)> >(o)> <(n)<"

allPosArgsAreFetchedMultipleMixesOne = standardTimeout 3 $ testCase name assertion
  where
    name = "A function declaration with mixed positional arg(s) is fetched appropriately"
    assertion = assertEqual d a f
    d = name
    a = assertArgs ["n", "o", "q"]
    f = getFunctionDeclPositionalArgs . noLineCalct $ "fish return_n >(n)> >(fish m <(1)<)> >(o)> >(fish p <(1)<)> >(q)> <(n)<"

allPosArgsAreFetchedMultipleMixesTwo = standardTimeout 3 $ testCase name assertion
  where
    name = "A function declaration with mixed positional arg(s) and a function declaration is fetched appropriately"
    assertion = assertEqual d a f
    d = name
    a = assertArgs ["n", "o", "q"]
    f = getFunctionDeclPositionalArgs . noLineCalct $ "fish return_n >(n)> >(fish m <(1)<)> >(o)> >(fish thing >(x)> <(x)<)> >(q)> <(n)<"

-----function execution tests------------------------------------------------------------------------

executesVerySimpleFunction = standardTimeout 3 $ testCase name assertion
  where
    name = "A very simple function can be defined and executed (1)"
    assertion = assertEqual d a f
    d = name
    a = Num 1.0
    f = prepareFunctionForTest $ "fish return_n >(n)> <(n)< swim <(return_n >(1)>)<"

executesVerySimpleFunctionTwo = standardTimeout 3 $ testCase name assertion
  where
    name = "A very simple function can be defined and executed (2)"
    assertion = assertEqual d a f
    d = name
    a = Num 2.0
    f = prepareFunctionForTest $ "fish add >(x)> >(y)> <(+ >(x)> >(y)>)< swim <(add >(1)> >(1)>)<"

executesMixedArgFunction = standardTimeout 3 $ testCase name assertion
  where
    name = "A very simple function can be defined and executed (3)"
    assertion = assertEqual d a f
    d = name
    a = Num 2.0
    f = prepareFunctionForTest $ "fish add >(x)> >(fish y <(1)<)> <(swim <(+ >(x)> >(y)>)<)< swim <(add >(1)>)<"

executesSingleValueFunction = standardTimeout 3 $ testCase name assertion
  where
    name = "A very simple function can be defined and executed (4)"
    assertion = assertEqual d a f
    d = name
    a = Num 5.0
    f = prepareFunctionForTest $ "fish return_five <(5)< swim <(return_five)<"

executesFunctionWithFin = standardTimeout 3 $ testCase name assertion
  where
    name = "A function with a fin call can be executed"
    assertion = assertEqual d a f
    d = name
    a = Boolean True
    f = prepareFunctionForTest $ "fish to_bool >(x)> <(fin >(x)> >(True)> >(False)>)< swim <(to_bool >(1)>)<"

executesFunctionWithNestedFin = standardTimeout 3 $ testCase name assertion
  where
    name = "A function with a nested fin call can be executed"
    assertion = assertEqual d a f
    d = name
    a = Boolean True
    f = prepareFunctionForTest $ "fish and >(x)> >(y)> <(fin >(x)> >(fin >(y)> >(True)> >(False)>)> >(False)>)< <(and >(True)> >(True)>)<"

executesFunctionWithSubFunc = standardTimeout 3 $ testCase name assertion
  where
    name = "A function with a sub-function definition can be executed"
    assertion = assertEqual d a f
    d = name
    a = Num 0.0
    f = prepareFunctionForTest $ "fish subtract_one >(x)> >(fish sub >(y)> <(- >(y)> >(1)>)<)> <(sub >(x)>)< <(subtract_one >(1)>)<"

simpleRecursiveFunction = standardTimeout 3 $ testCase name assertion
  where
    name = "A simple recursive function works"
    assertion = assertEqual d a f
    d = name
    a = Num 0.0
    f = prepareFunctionForTest $ "fish to_zero >(n)> <(fin >(== >(n)> >(0)> )> >(0)> >(to_zero >( - >(n)> >(1)>)>)>)< <(to_zero >(10)>)<"

functionsCallOtherFunctions = standardTimeout 3 $ testCase name assertion
  where
    name = "A function can call another function."
    assertion = assertEqual d a f
    d = name
    a = Num 5.0
    f = prepareFunctionForTest $ "fish id >(x)> <(x)< fish idd >(y)> <( id >(y)> )< <(idd >(5)>)<"

functionsCallOtherFunctionsTwo = standardTimeout 3 $ testCase name assertion
  where
    name = "A function can call another function."
    assertion = assertEqual d a f
    d = name
    a = Num 5.0
    f = prepareFunctionForTest $ "fish id >(x)> <(x)< fish idd >(y)> <( id >(y)> )< fish iddd >(z)> <(idd >(z)>)< <(iddd >(5)>)<"

functionsCallOtherFunctionsThree = standardTimeout 3 $ testCase name assertion
  where
    name = "A function can call another function, also, variable scope works appropriately between functions"
    assertion = assertEqual d a f
    d = name
    a = Num 5.0
    f = prepareFunctionForTest $ "fish id >(x)> <(x)< fish idd >(x)> <( id >(x)> )< <(idd >(5)>)<"

concurrentFunctionCallsWork = standardTimeout 3 $ testCase name assertion
  where
    name = "Function calls that take and return different values work appropriately."
    assertion = assertEqual d a f
    d = name
    a = Num 3.0
    f = prepareFunctionForTest $ "fish id >(x)> <(x)< <(+ >(id >(1)>)> >(id >(2)>)>)<"

concurrentFunctionCallsWorkTwo = standardTimeout 3 $ testCase name assertion
  where
    name = "Function calls that take and return different values work appropriately (null or 0 send fish)."
    assertion = assertEqual d a f
    d = name ++ " functions that take no arguments should be able to be called flexibly with a null or 0 send fish"
    a = Num 3.0
    f = prepareFunctionForTest $ "fish x <(1)< fish y <(2)< <(+ >(x >()>)> >(y)>)<"

functionsCallOtherFunctionsConcurrently = standardTimeout 3 $ testCase name assertion
  where
    name = "Concurrent function calls work for functions that call other functions."
    assertion = assertEqual d a f
    d = name
    a = Num 3.0
    f = prepareFunctionForTest $ "fish id >(x)> <(x)< fish idd >(x)> <( id >(x)> )< <(+ >(id >(1)>)> >(idd >(2)>)> )<"

simplePartialFunctionApplication = standardTimeout 3 $ testCase name assertion
  where
    name = "A function taking one argument returns a function that takes one argument and returns a value."
    assertion = assertEqual d a f
    d = name
    a = Num 2.0
    f = prepareFunctionForTest $ "fish add >(x)> <(fish add_ >(y)> <(+ >(x)> >(y)>)< )< <(add >(1)> >(1)>)<"

simpleShadowingOfId = standardTimeout 3 $ testCase name assertion
  where
    name = "A binding name can be shadowed in other scopes."
    assertion = assertEqual d a f
    d = name
    a = Num 2.0
    f = prepareFunctionForTest $ "fish x <(1)< fish id >(x)> <(x)< <(id >(2)>)<"

simpleShadowingOfIdTwo = standardTimeout 3 $ testCase name assertion
  where
    name = "A binding name can be shadowed in other scopes (Test two)."
    assertion = assertEqual d a f
    d = name
    a = Num 1.0
    f = prepareFunctionForTest $ "fish x <(1)< fish id >(x)> <(x)< <(id >(x >()> )>)<"

functionsWithNoExplicitSendFishAreAllowed = standardTimeout 3 $ testCase name assertion
  where
    name = "Functions can be defined with no explicit send fish."
    assertion = assertEqual d a f
    d = name ++ "Can be called without any send fish"
    a = Num 1.0
    f = prepareFunctionForTest $ "fish x <(1)< <(x)<"

functionsWithNoExplicitSendFishAreAllowedTwo = standardTimeout 3 $ testCase name assertion
  where
    name = "Functions without explicit send fish can be called with an empty send fish."
    assertion = assertEqual d a f
    d = name
    a = Num 1.0
    f = prepareFunctionForTest $ "fish x <(1)< <(x >()>)<"

functionsWithExplicitNoArgs = standardTimeout 3 $ testCase name assertion
  where
    name = "A function can be explicitly declared to take no arguments and can be called with no send fish."
    assertion = assertEqual d a f
    d = name
    a = Num 1.0
    f = prepareFunctionForTest $ "fish x >()> <(1)< <(x)<"

functionsWithExplicitNoArgsTwo = standardTimeout 3 $ testCase name assertion
  where
    name = "A function can be explicitly declared to take no arguments."
    assertion = assertEqual d a f
    d = name ++ "And can be called with an explicit or implicit send fish."
    a = Num 1.0
    f = prepareFunctionForTest $ "fish x >()> <(1)< <(x >()>)<"

functionCanReturnNull = standardTimeout 3 $ testCase name assertion
  where
    name = "A function can return a null value."
    assertion = assertEqual d a f
    d = name ++ "And can be called with an explicit or implicit send fish."
    a = Null
    f = prepareFunctionForTest $ "fish n <()< <(n >()>)<"

oneNestedFunctionCall = standardTimeout 3 $ testCase name assertion
  where
    name = "Nested function calls work as values when used in function arguments."
    assertion = assertEqual d a f
    d = name ++ "And can be called with an explicit or implicit send fish."
    a = Num 3.0
    f = prepareFunctionForTest $ fishCall "incr >(incr >(1)>)>"

tenNestedFunctionCall = standardTimeout 3 $ testCase name assertion
  where
    name = "Nested function calls work as values when used in function arguments."
    assertion = assertEqual d a f
    d = name ++ "And can be called with an explicit or implicit send fish."
    a = Num 10.0
    f = prepareFunctionForTest $ fishCall "incr >(incr >(incr >(incr >(incr >(incr >(incr >(incr >(incr >(incr >(0)>)>)>)>)>)>)>)>)>)>"

metaFishCallWorks = standardTimeout 3 $ testCase name assertion
  where
    name = "The wrapper FishCall works in the test suite"
    assertion = assertEqual d a f
    d = name
    a = Boolean False
    f = prepareFunctionForTest $ fishCall "and >(True)> >(False)>"

functionWithSwimWorks = standardTimeout 3 $ testCase name assertion
  where
    name = "A function with a swim keyword will still return the correct value."
    assertion = assertEqual d a f
    d = name
    a = Num 5.0
    f = prepareFunctionForTest "fish print_and_return >(x)> <(swim >(trout >(x)>)> <(x)<)< swim <(print_and_return >(5)>)<"

functionWithSwimWorksTwo = standardTimeout 3 $ testCase name assertion
  where
    name = "A function with a swim keyword will still return the correct value (2)."
    assertion = assertEqual d a f
    d = name
    a = Num 5.0
    f = prepareFunctionForTest "fish print_and_return >(x)> <(swim >(trout >(x)>)> >(trout >(\"Slightly more complex swim block\")>)> <(x)<)< swim <(print_and_return >(5)>)<"

subFunctionsExecutedAppriopriatelyWithSwim = standardTimeout 3 $ testCase name assertion
  where
    name = "A function with a defined sub-function will execute properly with a swim block."
    assertion = assertEqual d a f
    d = name
    a = Num 2.0
    f = prepareFunctionForTest "fish outer >(x)> >(fish inner >(y)> <(+ >(y)> >(1)>)<)> <(swim >(trout >(x)>)> <(inner >(x)>)<)< swim <(outer >(1)>)<"

subFunctionsExecutedAppriopriatelyWithSwimTwo = standardTimeout 3 $ testCase name assertion
  where
    name = "A function with a slight more complex defined sub-function will execute properly with a swim block."
    assertion = assertEqual d a f
    d = name
    a = Num 2.0
    f = prepareFunctionForTest "fish outer >(x)> >(fish inner >(y)> <(swim >(trout >(\"This sub-function should still return the right value\")>)> <(+ >(y)> >(1)>)<)<)> <(swim >(trout >(x)>)> <(inner >(x)>)<)< swim <(outer >(1)>)<"

simpleFishSendingBindingWorks = standardTimeout 3 $ testCase name assertion
  where
    name = "A fish send can be used to bind the result of a function to a scope."
    assertion = assertEqual d a f
    d = name
    a = Num 50.0
    f = prepareFunctionForTest "fish mult_ten >(x)> <(* >(x)> >(10)>)< swim >(result <(mult_ten >(5)>)<)> >(trout >(result)>)> <(result)<"

simpleFishSendingBindingWorksTwo = standardTimeout 3 $ testCase name assertion
  where
    name = "A fish send can be used to bind the result of a function to a scope (2)."
    assertion = assertEqual d a f
    d = name
    a = Num 150.0
    f = prepareFunctionForTest "fish mult_ten >(x)> <(* >(x)> >(10)>)< swim >(result <(mult_ten >(5)>)<)> >(result_two <(mult_ten >(10)>)<)> >(trout >(result)>)> <(+ >(result)> >(result_two)>)<"

simpleFishSendingBindingWorksThree = standardTimeout 3 $ testCase name assertion
  where
    name = "A fish send can be used to bind the result of a function to a scope (2)."
    assertion = assertEqual d a f
    d = name
    a = Num 150.0
    f = prepareFunctionForTest "fish mult_ten >(x)> <(* >(x)> >(10)>)< swim >(result <(mult_ten >(5)>)<)> >(result_two <(mult_ten >(10)>)<)> >(final <(+ >(result)> >(result_two)>)<)> >(trout >(result)>)> <(final)<"

subFunctionsWorkWithFishSend = standardTimeout 3 $ testCase name assertion
  where
    name = "Fish sends work in the main block when executing functions with sub functions."
    assertion = assertEqual d a f
    d = name
    a = Num 6.0
    f = prepareFunctionForTest "fish outer >(x)> >(fish inner >(y)> <(swim >(trout >(\"This sub-function should still return the right value\")>)> <(+ >(y)> >(1)>)<)<)> <(swim >(trout >(x)>)> <(inner >(x)>)<)< swim >(a <(outer >(1)>)<)> >(b <(outer >(2)>)<)> <(* >(a)> >(b)>)<"

subFunctionsWorkWithFishSendTwo = standardTimeout 3 $ testCase name assertion
  where
    name = "Fish sends work in the main block when executing functions with sub functions that have send fish."
    assertion = assertEqual d a f
    d = name
    a = Num 6.0
    f =
      prepareFunctionForTest
        "fish outer >(x)>\
        \ >(fish inner >(y)>\
        \<(swim >(trout >(\"This sub-function should still return the right value\")>)> <(+ >(y)> >(1)>)<)<)>\
        \ <(swim >(to_return <(inner >(x)>)<)> >(trout >(x)>)> <(to_return)<)<\
        \ swim >(a <(outer >(1)>)<)> >(b <(outer >(2)>)<)> <(* >(a)> >(b)>)<"

subFunctionsWorkWithFishSendThree = standardTimeout 3 $ testCase name assertion
  where
    name = "Fish sends work in the main block when executing functions with sub functions that have send fish."
    assertion = assertEqual d a f
    d = name
    a = Num 35.0
    f =
      prepareFunctionForTest
        "fish outer >(x)>\
        \ >(fish inner >(y)>\
        \<(swim >(trout >(\"This sub-function should still return the right value\")>)> <(+ >(y)> >(1)>)<)<)>\
        \ <(swim >(z <(* >(inner >(x)>)> >(2)>)<)> >(to_return <(inner >(z)>)<)> >(trout >(z)>)> <(to_return)<)<\
        \ swim >(a <(outer >(1)>)<)> >(b <(outer >(2)>)<)> <(* >(a)> >(b)>)<"

--outer 1 = 5
--outer 2 = 7
functionExecutionTests = testGroup "Function execution tests" testList
  where
    testList =
      [ subFunctionsWorkWithFishSendThree,
        subFunctionsWorkWithFishSendTwo,
        subFunctionsWorkWithFishSend,
        simpleFishSendingBindingWorksThree,
        simpleFishSendingBindingWorksTwo,
        simpleFishSendingBindingWorks,
        subFunctionsExecutedAppriopriatelyWithSwimTwo,
        subFunctionsExecutedAppriopriatelyWithSwim,
        functionWithSwimWorksTwo,
        functionWithSwimWorks,
        tenNestedFunctionCall,
        oneNestedFunctionCall,
        functionCanReturnNull,
        functionsWithExplicitNoArgsTwo,
        functionsWithExplicitNoArgs,
        functionsWithNoExplicitSendFishAreAllowedTwo,
        functionsWithNoExplicitSendFishAreAllowed,
        simpleShadowingOfIdTwo,
        executesVerySimpleFunction,
        executesVerySimpleFunctionTwo,
        executesMixedArgFunction,
        executesSingleValueFunction,
        executesFunctionWithFin,
        executesFunctionWithNestedFin,
        executesFunctionWithSubFunc,
        simpleRecursiveFunction,
        functionsCallOtherFunctions,
        functionsCallOtherFunctionsTwo,
        functionsCallOtherFunctionsThree,
        concurrentFunctionCallsWork,
        concurrentFunctionCallsWorkTwo,
        functionsCallOtherFunctionsConcurrently,
        simplePartialFunctionApplication,
        simpleShadowingOfId,
        metaFishCallWorks
      ]
