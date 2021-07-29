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

        ]

-- | Lexer.tokenize tests as t
