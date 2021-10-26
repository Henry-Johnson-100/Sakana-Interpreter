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

t' = Lexer.tokenize "+ >(1)> >(1)>"

fstBifunctorMap :: (a -> c) -> [(a, b)] -> [(c, b)]
fstBifunctorMap f tupAB = [(f a', b') | (a', b') <- tupAB]

satisfyFunction :: (t -> Bool) -> (t -> a) -> [t] -> [(a, [t])]
satisfyFunction _ _ [] = []
satisfyFunction f transform (x : xs) = if f x then [(transform x, xs)] else []

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
isData = Parser $ satisfyFunction isDataAndNotId id
  where
    isDataAndNotId d' =
      Util.foldIdApplicativeOnSingleton
        all
        [Like.like Lexer.genericData, not . Lexer.dataTokenIsId]
        (Lexer.unit d')

control :: C.Control -> Parser Lexer.TokenUnit
control c = Parser $ satisfyFunction (\tu -> (Lexer.unit tu) == (Lexer.Control c)) id

bracket :: B.ScopeType -> B.BracketTerminal -> Parser Lexer.TokenUnit
bracket sc bt =
  Parser $
    satisfyFunction (\tu -> (Lexer.unit tu) == (Lexer.Bracket sc bt)) id

anyOp :: Parser Lexer.TokenUnit
anyOp =
  Parser $ satisfyFunction (\tu -> ((Lexer.unit tu) `Like.like` Lexer.genericOperator)) id

bracketContents st = do
  bracket st B.Open
  contents <- expr
  bracket st B.Close
  return contents

opExpr = do
  op <- anyOp
  args <- some (bracketContents B.Send)
  return ((: concat args) op)

finExpr = do
  fin <- control C.Fin
  args <- some (bracketContents B.Send)
  return ((: concat args) fin)

swimExp = do
  swim <- keyword K.Swim
  procs <- many (bracketContents B.Send)
  value <- bracketContents B.Return
  return (swim : value ++ (concat procs))

funcCall = do
  calledId <- isId
  args <- many (bracketContents B.Send)
  return (calledId : (concat args))

expr = opExpr <|> finExpr <|> swimExp <|> funcCall <|> fmap (: []) isData