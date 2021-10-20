import Data.Maybe
import ExecutionTree
import Lexer
import SyntaxTree
import System.IO.Unsafe
import Test.Tasty
import Test.Tasty.HUnit
import Token.Data
import Token.Util.Tree

executeFirstChild :: String -> Data
executeFirstChild = unsafePerformIO . ExecutionTree.execute noEnvironmentStack . ExecutionTree.calct'

getMainTree :: String -> IO SyntaxTree
getMainTree = return . generateSyntaxTree . tokenize

prepareFunctionForTest str = unsafePerformIO $ executeMain (exEnv str) (exTr str)
  where
    docTree = SyntaxTree.generateSyntaxTree . Lexer.tokenize
    exEnv = return . getMainEnvironmentStack . docTree
    exTr = return . getMainExecutionTree . docTree

agnosticizeLines tr = fmap (setLineToZero) tr
  where
    setLineToZero (SyntaxUnit t _ c) = SyntaxUnit t 0 c

noLineCalct = agnosticizeLines . calct'

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
fishCall str = (fishEnv ++ " ") ++ ("<(" ++ str ++ ")<")

-- | main
main = do
  defaultMain tests

tests = testGroup "ExecutionTree tests" testList
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

oneFuncArgIsFetched = standardTimeout 3 $ testCase name assertion
  where
    name = "A function declaration with one positional arg(s) is fetched appropriately"
    assertion = assertEqual d a f
    d = name
    a = [noLineCalct] <*> [">(n)>"]
    f = getFuncDeclArgs . noLineCalct $ "fish return_n >(n)> <(n)<"

twoFuncArgIsFetched = standardTimeout 3 $ testCase name assertion
  where
    name = "A function declaration with two positional arg(s) is fetched appropriately"
    assertion = assertEqual d a f
    d = name
    a = [noLineCalct] <*> [">(n)>", ">(m)>"]
    f = getFuncDeclArgs . noLineCalct $ "fish return_n >(n)> >(m)> <(n)<"

threeFuncArgIsFetched = standardTimeout 3 $ testCase name assertion
  where
    name = "A function declaration with three positional arg(s) is fetched appropriately"
    assertion = assertEqual d a f
    d = name
    a = [noLineCalct] <*> [">(n)>", ">(m)>", ">(o)>"]
    f = getFuncDeclArgs . noLineCalct $ "fish return_n >(n)> >(m)> >(o)> <(n)<"

allPosArgsAreFetchedOne = standardTimeout 3 $ testCase name assertion
  where
    name = "A function declaration with three positional arg(s) is fetched appropriately"
    assertion = assertEqual d a f
    d = name
    a = [noLineCalct] <*> [">(n)>", ">(m)>", ">(o)>"]
    f = getFunctionDeclPositionalArgs . noLineCalct $ "fish return_n >(n)> >(m)> >(o)> <(n)<"

allPosArgsAreFetchedTwo = standardTimeout 3 $ testCase name assertion
  where
    name = "A function declaration with one positional arg(s) is fetched appropriately"
    assertion = assertEqual d a f
    d = name
    a = [noLineCalct] <*> [">(n)>"]
    f = getFunctionDeclPositionalArgs . noLineCalct $ "fish return_n >(n)> <(n)<"

allPosArgsAreFetchedThree = standardTimeout 3 $ testCase name assertion
  where
    name = "A function declaration with mixed positional arg(s) is fetched appropriately"
    assertion = assertEqual d a f
    d = name
    a = [noLineCalct] <*> [">(n)>", ">(o)>"]
    f = getFunctionDeclPositionalArgs . noLineCalct $ "fish return_n >(n)> >(m <(1)<)> >(o)> <(n)<"

allPosArgsAreFetchedMultipleMixesOne = standardTimeout 3 $ testCase name assertion
  where
    name = "A function declaration with mixed positional arg(s) is fetched appropriately"
    assertion = assertEqual d a f
    d = name
    a = [noLineCalct] <*> [">(n)>", ">(o)>", ">(q)>"]
    f = getFunctionDeclPositionalArgs . noLineCalct $ "fish return_n >(n)> >(m <(1)<)> >(o)> >(p <(1)<)> >(q)> <(n)<"

allPosArgsAreFetchedMultipleMixesTwo = standardTimeout 3 $ testCase name assertion
  where
    name = "A function declaration with mixed positional arg(s) and a function declaration is fetched appropriately"
    assertion = assertEqual d a f
    d = name
    a = [noLineCalct] <*> [">(n)>", ">(o)>", ">(q)>"]
    f = getFunctionDeclPositionalArgs . noLineCalct $ "fish return_n >(n)> >(m <(1)<)> >(o)> >(fish thing >(x)> <(x)<)> >(q)> <(n)<"

-- functionDisambiguationTests = testGroup "Function disambiguation tests" testList
--   where
--     testList =
--       [ simpleFunctionIsDisambiguated,
--         multiplePositionalArgsAreDisambiguated,
--         functionWithArgMixIsDisambiguatedOne,
--         functionWithArgMixIsDisambiguatedTwo,
--         functionWithArgMixIsDisambiguatedThree
--       ]

-- simpleFunctionIsDisambiguated = standardTimeout 3 $ testCase name assertion
--   where
--     name = "Simple function can be disambiguated"
--     assertion = assertEqual d a f
--     d = "Simple function can be disambiguated"
--     a = noLineCalct "fish return_n >(n <(1)<)> <(n)<"
--     f =
--       disambiguateFunction
--         noEnvironmentStack
--         (noLineCalct "return_n >(1)>")
--         (noLineCalct "fish return_n >(n)> <(n)<")

-- multiplePositionalArgsAreDisambiguated = standardTimeout 3 $ testCase name assertion
--   where
--     name = "function declaration with multiple pos. args is disambiguated"
--     assertion = assertEqual d a f
--     d = name
--     a = noLineCalct "fish add >(x <(1)<)> >(y <(2)<)> <(something)<"
--     f =
--       disambiguateFunction
--         noEnvironmentStack
--         (noLineCalct "add >(1)> >(2)>")
--         (noLineCalct "fish add >(x)> >(y)> <(something)<")

-- functionWithArgMixIsDisambiguatedOne = standardTimeout 3 $ testCase name assertion
--   where
--     name = "A function with a mix of positional args and sending data is disambiguated"
--     assertion = assertEqual d a f
--     d = name
--     a = noLineCalct "fish xyz >(x <(1)<)> >(y <(2)<)> >(z <(3)<)> <(z)<"
--     f = disambiguateFunction noEnvironmentStack (noLineCalct "xyz >(1)> >(2)>") (noLineCalct "fish xyz >(x)> >(y)> >(z <(3)<)> <(z)<")

-- functionWithArgMixIsDisambiguatedTwo = standardTimeout 3 $ testCase name assertion
--   where
--     name = "A function with a mix of positional args and sending data is disambiguated"
--     assertion = assertEqual d a f
--     d = name
--     a = noLineCalct "fish xyz >(x <(1)<)> >(y <(2)<)> >(z <(3)<)> <(z)<"
--     f = disambiguateFunction noEnvironmentStack (noLineCalct "xyz >(1)> >(3)>") (noLineCalct "fish xyz >(x)> >(y <(2)<)> >(z)> <(z)<")

-- functionWithArgMixIsDisambiguatedThree = standardTimeout 3 $ testCase name assertion
--   where
--     name = "A function with a mix of positional args and sending data is disambiguated"
--     assertion = assertEqual d a f
--     d = name
--     a = noLineCalct "fish xyz >(x <(1)<)> >(y <(2)<)> >(z <(3)<)> <(z)<"
--     f = disambiguateFunction noEnvironmentStack (noLineCalct "xyz >(2)> >(3)>") (noLineCalct "fish xyz >(x <(1)<)> >(y)> >(z)> <(z)<")

-- functionWithMultipleArgMixIsDisambiguatedOne = standardTimeout 3 $ testCase name assertion
--   where
--     name = "A function with a mix of positional args and sending data is disambiguated"
--     assertion = assertEqual d a f
--     d = name
--     a = noLineCalct "fish xyz >(x <(1)<)> >(y <(2)<)> >(z <(3)<)> >(a <(4)<)> >(b <(5)<)> <(z)<"
--     f = disambiguateFunction noEnvironmentStack (noLineCalct "xyz >(1)> >(3)> >(5)>") (noLineCalct "fish xyz >(x)> >(y <(2)<)> >(z)> >(a <(4)<)> >(b)> <(z)<")

executesVerySimpleFunction = standardTimeout 3 $ testCase name assertion
  where
    name = "A very simple function can be defined and executed"
    assertion = assertEqual d a f
    d = name
    a = Num 1.0
    f = prepareFunctionForTest $ "fish return_n >(n)> <(n)< <(return_n >(1)>)<"

executesVerySimpleFunctionTwo = standardTimeout 3 $ testCase name assertion
  where
    name = "A very simple function can be defined and executed"
    assertion = assertEqual d a f
    d = name
    a = Num 2.0
    f = prepareFunctionForTest $ "fish add >(x)> >(y)> <(+ >(x)> >(y)>)< <(add >(1)> >(1)>)<"

executesMixedArgFunction = standardTimeout 3 $ testCase name assertion
  where
    name = "A very simple function can be defined and executed"
    assertion = assertEqual d a f
    d = name
    a = Num 2.0
    f = prepareFunctionForTest $ "fish add >(x)> >(fish y >()> <(1)<)> <(+ >(x)> >(y)>)< <(add >(1)>)<"

executesFunctionWithFin = standardTimeout 3 $ testCase name assertion
  where
    name = "A function with a fin call can be executed"
    assertion = assertEqual d a f
    d = name
    a = Boolean True
    f = prepareFunctionForTest $ "fish to_bool >(x)> <(fin >(x)> >(True)> >(False)>)< <(to_bool >(1)>)<"

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

functionExecutionTests = testGroup "Function execution tests" testList
  where
    testList =
      [ tenNestedFunctionCall,
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
