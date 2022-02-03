import Data.Either
import Data.Maybe
import Interpreter.Main
import Parser.Main
import Parser.Syntax
import System.IO.Unsafe (unsafePerformIO)
import Test.Core
import Util.Classes
import Util.Tree

expectData :: Either SyntaxTree Data -> Data
expectData = either (error "Expected data but got a SyntaxTree.") id

expectTree :: Either SyntaxTree Data -> SyntaxTree
expectTree = either id (error "Expected a SyntaxTree but got Data.")

asNum :: Data -> Double
asNum d = fromMaybe (error ("Expecting a Num but got " ++ format d)) (unNum d)

asBool :: Data -> Bool
asBool d = fromMaybe (error ("Expecting a Boolean but got " ++ format d)) (unBoolean d)

asString :: Data -> String
asString d = fromMaybe (error ("Expecting a String but got " ++ format d)) (unString d)

evaluateString :: String -> Either SyntaxTree Data
evaluateString = unsafePerformIO . flip evaluateProgram [] . parse "TestString"

inMain :: String -> String
inMain = (++) "fish main <( " . flip (++) " )<"

main =
  defaultMain . testGroup "Interpreter Tests" $
    [ primitiveDataTests,
      primitiveStdLibFunctionTests,
      functionDefinitionTests,
      curriedFunctionTests
    ]

primitiveDataTests :: TestTree
primitiveDataTests =
  testGroup
    "Primitive Data Tests"
    [ timedAssertEqual
        1
        "A program returns a constant primitive value"
        []
        (Boolean True)
        ((expectData . evaluateString . inMain) "True"),
      timedAssertEqual
        1
        "A program returns a constant primitive value"
        []
        (String "Hello World!")
        ((expectData . evaluateString . inMain) "\"Hello World!\"")
    ]

primitiveStdLibFunctionTests :: TestTree
primitiveStdLibFunctionTests =
  testGroup
    "Primitive Standard Library Function Tests"
    [ timedAssertEqual
        1
        "Binary addition of primitive numbers"
        []
        (Num 2.0)
        ((expectData . evaluateString . inMain) "+ >(1)> >(1)>"),
      timedAssertEqual
        1
        "Summation of multiple primitive numbers"
        []
        (Num 6.0)
        ((expectData . evaluateString . inMain) "+ >(1)> >(2)> >(3)>"),
      timedAssertEqual
        1
        "Associative subtraction of multiple primitive numbers"
        []
        (Num 4.0)
        ((expectData . evaluateString . inMain) "- >(10)> >(1)> >(2)> >(3)>"),
      timedAssertEqual
        1
        "A primitive number is parsed from a string"
        []
        (Num 25.25)
        ((expectData . evaluateString . inMain) "read_prim >(\"25.25\")>"),
      timedAssertEqual
        1
        "A primitive value can be cast to a string with show"
        []
        (String "25.25")
        ((expectData . evaluateString . inMain) "show_prim >(25.25)>"),
      timedAssertEqual
        1
        "A std algebraic folding function returns its argument if there is only one"
        []
        (Num 3.0)
        ((expectData . evaluateString . inMain) "/ >(3)>")
    ]

functionDefinitionTests :: TestTree
functionDefinitionTests =
  testGroup
    "Sakana Function tests"
    [ timedAssertEqual
        1
        "A simple function can be defined and called"
        []
        (Num 1.0)
        ( (expectData . evaluateString)
            "fish return_one\
            \ <(1)<\
            \\
            \fish main\
            \ <(return_one)<"
        ),
      timedAssertEqual
        1
        "A function can take an argument and return it"
        []
        (Num 1.0)
        ( (expectData . evaluateString)
            "fish identity >(x)>\
            \ <(x)<\
            \\
            \fish main\
            \ <(identity >(1)> )<"
        ),
      timedAssertEqual
        1
        "A function can take two arguments"
        []
        (Num 2.0)
        ( (expectData . evaluateString)
            "fish const >(x)> >(y)>\
            \ <(x)<\
            \\
            \fish main\
            \ <(const >(2)> >(0)>)<"
        ),
      timedAssertEqual
        1
        "A function can recall two arguments and operate with them"
        []
        (Num 2.0)
        ( (expectData . evaluateString)
            "fish add >(x)> >(y)>\
            \ <(+ >(x)> >(y)>)<\
            \\
            \fish main\
            \ <(add >(1)> >(1)>)<"
        ),
      timedAssertEqual
        1
        "A function can call other functions"
        []
        (Num 2.0)
        ( (expectData . evaluateString)
            "fish id >(x)> <(x)<\
            \fish add >(y)> >(z)> <(+ >(id >(y)>)> >(id >(z)>)>)<\
            \fish main <(add >(1)> >(1)>)<"
        ),
      timedAssertEqual
        1
        "A simple fin expression"
        []
        (Boolean False)
        ( (expectData . evaluateString)
            "fish not >(a)>\
            \ <( fin >(a)> >(False)> >(True)> )<\
            \\
            \fish main <(not >(True)>)<"
        ),
      timedAssertEqual
        1
        "A fin expression evaluates its arguments"
        []
        (Num 1.0)
        ( (expectData . evaluateString)
            "fish int_bool >(x)>\
            \ <(fin >(== >(x)> >(2)>)> >(- >(x)> >(1)>)> >(+ >(x)> >(1000)>)> )<\
            \fish main <(int_bool >(2)>)<"
        ),
      timedAssertEqual
        1
        "A simple recursive function"
        []
        (Num 0.0)
        ( (expectData . evaluateString)
            "fish to_zero >(x)>\
            \ <(fin >( <= >(x)> >(0)> )> >(0)> >(to_zero >(- >(x)> >(1)>)> )> )<\
            \fish main <(to_zero >(10)>)<"
        ),
      timedAssertEqual
        1
        "Definition of factorial"
        []
        (factorial 30)
        ( (asNum . expectData . evaluateString)
            "fish fact >(n)>\
            \ <(fin\
            \   >( <= >(n)> >(0)> )>\
            \   >(1)>\
            \   >(* >(n)> >(fact >(- >(n)> >(1)>)>)>)>\
            \ )<\
            \fish main <(fact >(30)>)<"
        )
    ]
  where
    factorial :: Double -> Double
    factorial n = if n <= 0 then 1 else (*) n (factorial ((-) n 1))

curriedFunctionTests :: TestTree
curriedFunctionTests =
  testGroup
    "Curried Sakana Function Tests"
    []