module ParseTree(
ParseTree(..),
generateParseTree
) where

import Lexer
import Token.Bracket
import Token.Control
import Token.Data
import Token.Keyword
import Token.Operator
import Token.Util.NestedCollapsible
import Data.List

data ParseTree a = Empty | Node a (ParseTree a) [ParseTree a] deriving (Show, Read, Eq)
type TokenTree = ParseTree Token

node :: a -> ParseTree a
node x = Node x Empty []


generateParseTree :: [Token] -> ParseTree Token
generateParseTree ts = Empty