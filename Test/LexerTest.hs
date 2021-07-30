import Test.Tasty
import Test.Tasty.HUnit
import Data.List

import Lexer
import qualified Token.Bracket    as B
import qualified Token.Control    as C
import qualified Token.Data       as D
import qualified Token.Keyword    as K
import qualified Token.Operator   as O


-- | main
main = do
    defaultMain tests

-- | All EC tests
tests = testGroup "Lexer Tests" testList where
    testList =
        [
            tokenize_add_one,
            tokenize_factorial,
            tokenize_factorial_unoptimal_spacing,
            tokenizeer_also_auto_consolidates_string_data
        ]

-- | Lexer.tokenize tests as t
tokenize_add_one = testCase name assertion where
    name      = "tokenize the function 'add_one'"
    assertion = assertEqual d a f
    d         = "fish add_one >( n )> <( n + 1 )<"
    a         = [Keyword K.Fish, Data (D.Id "add_one"), Bracket (B.Send B.Open), Data (D.Id "n"), Bracket (B.Send B.Close), Bracket (B.Return B.Open), Data (D.Id "n"), Operator O.Add, Data (D.Int 1), Bracket (B.Return B.Close)]
    f         = tokenize "fish add_one >(n)> <(n+1)<"

tokenize_factorial = testCase name assertion where
    name      = "tokenize the factorial function"
    assertion = assertEqual d a f
    d         = "fish factorial >(n)> <( fin >( n == 0 , 1 )> n * factorial >( <( n - 1 )< )> )<"
    a         = [Keyword K.Fish, Data (D.Id "factorial"), Bracket (B.Send B.Open), Data (D.Id "n"), Bracket (B.Send B.Close), Bracket (B.Return B.Open), Control C.Fin, Bracket (B.Send B.Open), Data (D.Id "n"), Operator O.Eq, Data (D.Int 0), Data (D.Punct ","), Data (D.Int 1), Bracket (B.Send B.Close), Data (D.Id "n"), Operator O.Mult, Data (D.Id "factorial"), Bracket (B.Send B.Open), Bracket (B.Return B.Open), Data (D.Id "n"), Operator O.Sub, Data (D.Int 1), Bracket (B.Return B.Close), Bracket (B.Send B.Close), Bracket (B.Return B.Close)]
    f         = tokenize "fish factorial >(n)> <( fin >( n == 0 , 1 )> n * factorial >( <( n - 1 )< )> )<"

tokenize_factorial_unoptimal_spacing = testCase name assertion where
    name      = "factorial function with no spacing between brackets, controls, operators, function or id calls"
    assertion = assertEqual d a f
    d         = "fish factorial >(n)><(fin>(n==0,1)>n*factorial>(<(n-1)<)>)<"
    a         = [Keyword K.Fish, Data (D.Id "factorial"), Bracket (B.Send B.Open), Data (D.Id "n"), Bracket (B.Send B.Close), Bracket (B.Return B.Open), Control C.Fin, Bracket (B.Send B.Open), Data (D.Id "n"), Operator O.Eq, Data (D.Int 0), Data (D.Punct ","), Data (D.Int 1), Bracket (B.Send B.Close), Data (D.Id "n"), Operator O.Mult, Data (D.Id "factorial"), Bracket (B.Send B.Open), Bracket (B.Return B.Open), Data (D.Id "n"), Operator O.Sub, Data (D.Int 1), Bracket (B.Return B.Close), Bracket (B.Send B.Close), Bracket (B.Return B.Close)]
    f         = tokenize "fish factorial >(n)><(fin>(n==0,1)>n*factorial>(<(n-1)<)>)<"

tokenizeer_also_auto_consolidates_string_data = testCase name assertion where
    name      = "groups of Data should be auto consolidated into one Data String if possible"
    assertion = assertEqual d a f
    d         = "fish string_concatter >(middle)> <(fin >(not >(null >(middle)>)>, \"beginning second \" + middle + \" end\")> \"beginning second end\")<"
    a         = [Keyword K.Fish, Data (D.Id "string_concatter"), Bracket (B.Send B.Open), Data (D.Id "middle"), Bracket (B.Send B.Close), Bracket (B.Return B.Open), Control C.Fin, Bracket (B.Send B.Open), Data (D.Id "not"), Bracket (B.Send B.Open), Data (D.Id "null"), Bracket (B.Send B.Open), Data (D.Id "middle"), Bracket (B.Send B.Close), Bracket (B.Send B.Close), Data (D.Punct ","), Data (D.String "\"beginning second \""), Operator O.Add, Data (D.Id "middle"), Operator O.Add, Data (D.String "\" end\""), Bracket (B.Send B.Close), Data (D.String "\"beginning second end\""), Bracket (B.Return B.Close)]
    f         = tokenize "fish string_concatter >(middle)> <(fin >(not >(null >(middle)>)>, \"beginning second \" + middle + \" end\")> \"beginning second end\")<"