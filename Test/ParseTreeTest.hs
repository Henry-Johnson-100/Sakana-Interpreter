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
    a         = Node (Keyword Fish) (Node (Data (Id "add_one")) (Node (Operator Add) Empty (Node (Data (Id "n")) (Node (Data (Int 1)) Empty Empty) Empty )) (Node (Data (Id "n")) Empty Empty)) Empty
    f         = generateParseTree $ L.tokenize "fish add_one >(n)> <(+ >(n,1)>)<"

gST_generates_appopriate_tree_for_fish_factorial = testCase name assertion where
    name      = "Parse tree for factorial"
    assertion = assertEqual d a f
    d         = "Should generate an appropriate tree from only the information contained in the Token list from tokenizing the add_one function"
    a         = Node (Keyword Fish) (Node (Data (Id "factorial")) (Node (Control Fin) (Node (Operator Mult) (Empty) (Node (Data (Id "n")) (Node (Data (Id "factorial")) (Empty) (Node (Operator Sub) (Empty) (Node (Data (Id "n")) (Node (Data (Int 1)) (Empty) (Empty)) (Empty)))) (Empty))) (Node (Operator Eq) (Node (Data (Int 1)) (Empty) (Empty)) (Node (Data (Id "n")) (Node (Data (Int 0)) (Empty) (Empty)) (Empty)))) (Node (Data (Id "n")) Empty Empty)) Empty
    f         = generateParseTree $ L.tokenize "fish factorial >(n)> <(fin >(== <(n,0)<, 1)> <(* >(n, <(factorial >(<(- >(n,1)>)<)>)<)>)<)<"