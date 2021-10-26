module SakanaParserCombinator
  (
  )
where

-- import Text.Parsec.Prim as Parsec.Prim

import Control.Applicative
import qualified Data.List as DList
import qualified Data.Maybe as DMaybe (Maybe (..), fromJust)
import qualified Exception.Base as Exception
import qualified Lexer
import qualified SyntaxTree
  ( SyntaxTree (..),
    SyntaxUnit (..),
    genericSyntaxUnit,
    setContext,
    tokenUnitToSyntaxUnit,
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

t' = Lexer.tokenize "fish add >(n)> >(m)> <(+ >(n)> >(m)>)<"

fstBifunctorMap :: (a -> c) -> [(a, b)] -> [(c, b)]
fstBifunctorMap f tupAB = [(f a', b') | (a', b') <- tupAB]

makeParserFunction :: (t -> Bool) -> (t -> a) -> [t] -> [(a, [t])]
makeParserFunction _ _ [] = []
makeParserFunction f transform (x : xs) = if f x then [(transform x, xs)] else []

determinedResult :: [(a, xs)] -> DMaybe.Maybe a
determinedResult [] = DMaybe.Nothing
determinedResult ((a, xs) : _) = DMaybe.Just a

ioDeterminedTree :: (Show a) => [(Tree.Tree a, x)] -> IO ()
ioDeterminedTree = Tree.ioPrintTree . DMaybe.fromJust . determinedResult

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
isData = Parser $ makeParserFunction isDataAndNotId id
  where
    isDataAndNotId d' =
      Util.foldIdApplicativeOnSingleton
        all
        [Like.like Lexer.genericData, not . Lexer.dataTokenIsId]
        (Lexer.unit d')

control :: C.Control -> Parser Lexer.TokenUnit
control c = Parser $ makeParserFunction (\tu -> (Lexer.unit tu) == (Lexer.Control c)) id

bracket :: B.ScopeType -> B.BracketTerminal -> Parser Lexer.TokenUnit
bracket sc bt =
  Parser $
    makeParserFunction (\tu -> (Lexer.unit tu) == (Lexer.Bracket sc bt)) id

anyOp :: Parser Lexer.TokenUnit
anyOp =
  Parser $
    makeParserFunction (\tu -> ((Lexer.unit tu) `Like.like` Lexer.genericOperator)) id

bracketContents :: B.ScopeType -> Parser (SyntaxTree.SyntaxTree)
bracketContents st = do
  bracket st B.Open
  contents <- expr st
  bracket st B.Close
  return (fmap (SyntaxTree.setContext st) contents)

opExpr :: B.ScopeType -> Parser (SyntaxTree.SyntaxTree)
opExpr st = do
  op <- anyOp
  args <- some (bracketContents B.Send)
  return ((Tree.tree . flip SyntaxTree.tokenUnitToSyntaxUnit st) op -<= args)

finExpr :: B.ScopeType -> Parser (SyntaxTree.SyntaxTree)
finExpr st = do
  fin <- control C.Fin
  args <- some (bracketContents B.Send)
  return ((Tree.tree . flip SyntaxTree.tokenUnitToSyntaxUnit st) fin -<= args)

swimExp :: B.ScopeType -> Parser (SyntaxTree.SyntaxTree)
swimExp st = do
  swim <- keyword K.Swim
  procs <- many (bracketContents B.Send <|> fishSend)
  value <- bracketContents B.Return
  return ((Tree.tree . flip SyntaxTree.tokenUnitToSyntaxUnit st) swim -<= procs -<- value)

fishSend :: Parser (SyntaxTree.SyntaxTree)
fishSend = do
  bracket B.Send B.Open
  bindId <- isId
  bracket B.Return B.Open
  bindValue <- expr B.Return
  bracket B.Return B.Close
  bracket B.Send B.Close
  return $ (Tree.tree . flip SyntaxTree.tokenUnitToSyntaxUnit B.Send) bindId -<- bindValue

funcCall :: B.ScopeType -> Parser (SyntaxTree.SyntaxTree)
funcCall st = do
  calledId <- isId
  args <- many (bracketContents B.Send)
  return ((Tree.tree . flip SyntaxTree.tokenUnitToSyntaxUnit st) calledId -<= args)

dataToTree :: B.ScopeType -> Parser (SyntaxTree.SyntaxTree)
dataToTree st = fmap (tokenUnitToTree) isData
  where
    tokenUnitToTree tu =
      Tree.tree $ SyntaxTree.SyntaxUnit (Lexer.unit tu) (Lexer.unitLine tu) st

expr :: B.ScopeType -> Parser (SyntaxTree.SyntaxTree)
expr st = opExpr st <|> finExpr st <|> swimExp st <|> funcCall st <|> dataToTree st

isIdTree :: B.ScopeType -> Parser (Tree SyntaxTree.SyntaxUnit)
isIdTree st = do
  idToTree <- isId
  return . Tree.tree . flip SyntaxTree.tokenUnitToSyntaxUnit st $ idToTree

funcDecl :: B.ScopeType -> Parser (Tree SyntaxTree.SyntaxUnit)
funcDecl st = do
  fish <- keyword K.Fish
  funcId <- isId
  args <- many (funcDeclArg)
  value <- bracketContents B.Return
  return $
    (Tree.tree . flip SyntaxTree.tokenUnitToSyntaxUnit st) fish
      -<- (Tree.tree . flip SyntaxTree.tokenUnitToSyntaxUnit B.Send) funcId
      -<= args
      -<- value

funcDeclArg = do
  bracket B.Send B.Open
  argContent <- isIdTree B.Send <|> funcDecl B.Send
  bracket B.Send B.Close
  return argContent