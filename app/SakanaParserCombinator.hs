module SakanaParserCombinator
  (
  )
where

-- import Text.Parsec.Prim as Parsec.Prim

import Control.Applicative
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
import qualified Util.Like as Like (Like (..))
import Util.Tree (Tree (..), (-<-), (-<=))
import qualified Util.Tree as Tree

newtype Parser a = Parser {parse :: [Lexer.TokenUnit] -> [(a, [Lexer.TokenUnit])]}

-- Shamelessly aped these instances from:
-- http://dev.stephendiehl.com/fun/002_parsers.html

instance Functor Parser where
  fmap f (Parser pf) = Parser (\s -> fstBifunctorMap f (pf s))

instance Applicative Parser where
  pure x = Parser (\s -> [(x, s)])
  (Parser pf1) <*> (Parser pf2) =
    --lol it's gonna take some time to figure this one out desu.
    Parser (\s -> [(f a, s'') | (f, s') <- pf1 s, (a, s'') <- pf2 s'])

instance Monad Parser where -- lol
  return = pure
  (>>=) p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

instance Alternative Parser where
  empty = Parser (\x -> [])
  pa <|> pb = Parser (\x -> case parse pa x of [] -> parse pb x; other -> other)

t' = Lexer.tokenize "fish hello >(x)> <(\"hello\")<"

fstBifunctorMap :: (a -> c) -> [(a, b)] -> [(c, b)]
fstBifunctorMap f tupAB = [(f a', b') | (a', b') <- tupAB]

-- Primitive parsers----------------------------------------------------------------------
------------------------------------------------------------------------------------------

keyword :: K.Keyword -> Parser Lexer.TokenUnit
keyword k =
  Parser
    ( \(tu : tus) ->
        if (Lexer.unit tu) == (Lexer.Keyword k)
          then [(tu, tus)]
          else []
    )

isId :: Parser Lexer.TokenUnit
isId =
  Parser
    ( \(tu : tus) ->
        if Lexer.dataTokenIsId (Lexer.unit tu)
          then [(tu, tus)]
          else []
    )

isData :: Parser Lexer.TokenUnit
isData = Parser (\(tu : tus) -> if isDataAndNotId tu then [(tu, tus)] else [])
  where
    isDataAndNotId d' =
      Util.foldIdApplicativeOnSingleton
        all
        [Like.like Lexer.genericData, not . Lexer.dataTokenIsId]
        (Lexer.unit d')

control :: C.Control -> Parser Lexer.TokenUnit
control c =
  Parser
    ( \(tu : tus) ->
        if (Lexer.unit tu) == (Lexer.Control c) then [(tu, tus)] else []
    )

bracket :: B.ScopeType -> B.BracketTerminal -> Parser Lexer.TokenUnit
bracket sc bt =
  Parser
    ( \(tu : tus) ->
        if (Lexer.unit tu) == (Lexer.Bracket sc bt) then [(tu, tus)] else []
    )

anyOp :: Parser Lexer.TokenUnit
anyOp =
  Parser
    ( \(tu : tus) ->
        if (Lexer.unit tu) `Like.like` Lexer.genericOperator then [(tu, tus)] else []
    )