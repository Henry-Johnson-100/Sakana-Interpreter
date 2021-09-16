import Test.Tasty
import Test.Tasty.HUnit
import ParseTree as PT
import Lexer as L
import Token.Bracket
import Token.Control
import Token.Data
import Token.Keyword
import Token.Operator

main = do
    defaultMain tests

tests = testGroup "ParseTree Tests" testList where
    testList =
        [
            gSTTests
        ]

-- | generateParseTree as gST
gSTTests = testGroup "PT.generateParseTree tests" testList where
    testList =
        [
            gST_generates_appropriate_tree_for_fish_add_one,
            gST_generates_appopriate_tree_for_fish_factorial
        ]

gST_generates_appropriate_tree_for_fish_add_one = testCase name assertion where
    name      = "Parse tree for add_one"
    assertion = assertEqual d a f
    d         = "Should generate an appropriate tree from only the information contained in the Token list from tokenizing the add_one function"
    a         = ParseTree (Data (Id "add_one")) [ParseTree (Data (Id "n")) [], ParseTree (Operator Add) [ParseTree (Data (Id "n")) [], ParseTree (Data (Int 1)) []]]
    f         = generateParseTree $ L.tokenize "fish add_one >(n)> <(+ >(n,1)>)<"

gST_generates_appopriate_tree_for_fish_factorial = testCase name assertion where
    name      = "Parse tree for factorial"
    assertion = assertEqual d a f
    d         = "Should generate an appropriate tree from only the information contained in the Token list from tokenizing the add_one function"
    a         = ParseTree (Data (Id "factorial")) [ParseTree (Data (Id "n")) [], ParseTree (Control Fin) [ParseTree (Operator Eq) [ParseTree (Data (Id "n")) [], ParseTree (Data (Int 0)) []], ParseTree (Data (Int 1)) []], ParseTree (Operator Mult) [ParseTree (Data (Id "n")) [ParseTree (Data (Id "factorial")) [ParseTree (Operator Sub) [ParseTree (Data (Id "n")) [], ParseTree (Data (Int 1)) []]]]]]
    f         = generateParseTree $ L.tokenize "fish factorial >(n)> <(fin >(== >(n,0)>, 1)> * >(n, <(factorial >(- >(n,1)>)>)<)>)<"