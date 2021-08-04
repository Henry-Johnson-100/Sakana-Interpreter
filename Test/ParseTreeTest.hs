import Test.Tasty
import Test.Tasty.HUnit
import ParseTree as PT
import Lexer as L

main = do
    defaultMain tests

tests = testGroup "ParseTree Tests" testList where
    testList =
        [
            gSTTests
        ]

-- | generateScopedTreeFromTokens as gST
gSTTests = testGroup "PT.generateScopedTreeFromTokens tests" testList where
    testList =
        [
            gST_generates_appropriate_tree_for_fish_add_one,
            gST_generates_appopriate_tree_for_fish_factorial
        ]
gST_generates_appropriate_tree_for_fish_add_one = testCase name assertion where
    name      = "Parse tree for add_one"
    assertion = assertEqual d a f
    d         = "Should generate an appropriate tree from only the information contained in the Token list from tokenizing the add_one function"
    a         = Empty
    f         = generateScopedTreeFromTokens $ L.tokenize "fish add_one >(n)> <(n+1)<"

gST_generates_appopriate_tree_for_fish_factorial = testCase name assertion where
    name      = "Parse tree for factorial"
    assertion = assertEqual d a f
    d         = "Should generate an appropriate tree from only the information contained in the Token list from tokenizing the add_one function"
    a         = Empty
    f         = generateScopedTreeFromTokens $ L.tokenize "fish factorial >(n)> <(fin >(n==0, 1)> <(n*factorial>(<(n-1)<)>)<"