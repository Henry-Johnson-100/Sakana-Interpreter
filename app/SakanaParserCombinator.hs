module SakanaParserCombinator
  (
  )
where

-- import Text.Parsec.Prim as Parsec.Prim

import qualified Data.List as DList
import qualified Exception.Base as Exception
import qualified Lexer
import qualified SyntaxTree
  ( SyntaxTree (..),
    SyntaxUnit (..),
    genericSyntaxUnit,
    setContext,
  )
import qualified Token.Bracket as B
import qualified Token.Control as C
import qualified Token.Data as D
import qualified Token.Keyword as K
import qualified Token.Operator as O
import qualified Util.General as Util
import qualified Util.Like as Like (Like(..))
import Util.Tree (Tree (..), (-<-), (-<=))
import qualified Util.Tree as Tree

newtype Parser a = Parser {parse :: [Lexer.TokenUnit] -> [(a, [Lexer.TokenUnit])]}

-- Shamelessly aped these instances from:
-- http://dev.stephendiehl.com/fun/002_parsers.html

instance Functor Parser where
  fmap f (Parser pf) = Parser (\s -> [(f a, b) | (a, b) <- pf s])

instance Applicative Parser where
  pure x = Parser (\s -> [(x, s)])
  (Parser pf1) <*> (Parser pf2) =
    --lol it's gonna take some time to figure this one out desu.
    Parser (\s -> [(f a, s'') | (f, s') <- pf1 s, (a, s'') <- pf2 s'])

instance Monad Parser where -- lol
  return = pure
  (>>=) p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

  