{-# LANGUAGE MagicHash #-}

-- #TODO institute error checking for nodes containing primitive data types
-- They should not have children trees, if so, they will probably be ignored

-- binary operators should only have two children
module SyntaxTree
  ( SyntaxTree,
    Tree.TreeIO,
    SyntaxUnit (..),
    generateSyntaxTree,
    -- generateModuleTree,
    genericSyntaxUnit,
    getSyntaxAttributeFromTree,
    nthChildMeetsCondition,
    setContext,
    tokenUnitToSyntaxUnit,
  )
where

import Control.Applicative (Alternative (empty, many, some, (<|>)))
import qualified Data.List (intercalate)
import qualified Data.Maybe as DMaybe (Maybe (Just, Nothing), fromJust)
import qualified Exception.Base
  ( ExceptionSeverity (Fatal, NonFatal),
    ExceptionType (DeclarationMissingId, FreeTokensInForeignScope),
    newException,
    raiseError,
  )
import qualified Lexer
  ( PacketUnit (PacketUnit, unit, unitLine),
    Token (Bracket, Control, Data, Keyword),
    TokenUnit,
    dataTokenIsId,
    fromToken,
    genericData,
    genericOperator,
    getTokenBracketScopeType,
    keywordTokenIsDeclarationRequiringId,
  )
import qualified Token.Bracket as B
  ( BracketTerminal (Close, Open),
    ScopeType (..),
  )
import qualified Token.Control as C
import qualified Token.Data as D (Data (Id, Null))
import qualified Token.Keyword as K (Keyword (Fish, School, Swim))
import qualified Util.CollapsibleTerminalCases as CTC (CollapsibleTerminalCases (..))
import qualified Util.EagerCollapsible as EagerCollapsible (dropInfix)
import qualified Util.General as Util
import qualified Util.Like as Like
import qualified Util.NestedCollapsible as NestedCollapsible
  ( TriplePartition (..),
    breakByNest,
    groupByPartition,
    hasNestedCollapsible,
    isCompleteNestedCollapsible,
    nestedCollapsibleIsPrefixOf,
    takeNestWhileComplete,
    takeWhileList,
  )
import Util.Tree (Tree ((:-<-:)), (-<-), (-<=))
import qualified Util.Tree as Tree
  ( Tree (Empty),
    TreeIO (..),
    childMap,
    childrenOfChildren,
    lookupOn,
    maybeOnTreeNode,
    mutateTreeNode,
    reTree,
    serialTree,
    tree,
    treeChildren,
    treeNode,
  )

data SyntaxUnit = SyntaxUnit
  { token :: Lexer.Token,
    line :: Int,
    context :: B.ScopeType
  }
  deriving (Show, Eq)

genericSyntaxUnit :: Lexer.Token -> SyntaxUnit
genericSyntaxUnit t = SyntaxUnit t 0 B.Return

setContext :: B.ScopeType -> SyntaxUnit -> SyntaxUnit
setContext st su = su {context = st}

type SyntaxTree = Tree.Tree SyntaxUnit

type SyntaxPartition = NestedCollapsible.TriplePartition SyntaxUnit

tokenUnitToSyntaxUnit :: Lexer.TokenUnit -> B.ScopeType -> SyntaxUnit
tokenUnitToSyntaxUnit tu = SyntaxUnit (Lexer.unit tu) (Lexer.unitLine tu)

generateSyntaxTree :: [Lexer.TokenUnit] -> SyntaxTree
generateSyntaxTree = (fst . head . parse program)

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

--Except for this one, this one is mine :^)
instance Alternative Parser where
  empty = Parser (\x -> [])
  pa <|> pb = Parser (\x -> case parse pa x of [] -> parse pb x; other -> other)

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
keyword k = Parser $ makeParserFunction (\tu -> (Lexer.unit tu) == (Lexer.Keyword k)) id

isId :: Parser Lexer.TokenUnit
isId = Parser $ makeParserFunction (\tu -> (Lexer.dataTokenIsId (Lexer.unit tu))) id

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

bracketContainingExpr :: B.ScopeType -> Parser (SyntaxTree.SyntaxTree)
bracketContainingExpr st = do
  bracket st B.Open
  contents <- expr st
  bracket st B.Close
  return (fmap (SyntaxTree.setContext st) contents)

opExpr :: B.ScopeType -> Parser (SyntaxTree.SyntaxTree)
opExpr st = do
  op <- anyOp
  args <- some (bracketContainingExpr B.Send)
  return ((Tree.tree . flip SyntaxTree.tokenUnitToSyntaxUnit st) op -<= args)

finExpr :: B.ScopeType -> Parser (SyntaxTree.SyntaxTree)
finExpr st = do
  fin <- control C.Fin
  args <- some (bracketContainingExpr B.Send)
  return ((Tree.tree . flip SyntaxTree.tokenUnitToSyntaxUnit st) fin -<= args)

swimExp :: B.ScopeType -> Parser (SyntaxTree.SyntaxTree)
swimExp st = do
  swim <- keyword K.Swim
  procs <- many (bracketContainingExpr B.Send <|> fishSend)
  value <- bracketContainingExpr B.Return
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
  args <- many (bracketContainingExpr B.Send)
  return ((Tree.tree . flip SyntaxTree.tokenUnitToSyntaxUnit st) calledId -<= args)

expr :: B.ScopeType -> Parser (SyntaxTree.SyntaxTree)
expr st = opExpr st <|> finExpr st <|> swimExp st <|> funcCall st <|> dataToTree st
  where
    dataToTree :: B.ScopeType -> Parser (SyntaxTree.SyntaxTree)
    dataToTree st = fmap (Tree.tree . flip SyntaxTree.tokenUnitToSyntaxUnit st) isData

statement :: B.ScopeType -> Parser (Tree SyntaxTree.SyntaxUnit)
statement st = funcDecl st

funcDecl :: B.ScopeType -> Parser (Tree SyntaxTree.SyntaxUnit)
funcDecl st = do
  fish <- keyword K.Fish
  funcId <- isId
  args <- many (funcDeclArg)
  value <- bracketContainingExpr B.Return
  return $
    (Tree.tree . flip SyntaxTree.tokenUnitToSyntaxUnit st) fish
      -<- (Tree.tree . flip SyntaxTree.tokenUnitToSyntaxUnit B.Send) funcId
      -<= args
      -<- value
  where
    funcDeclArg :: Parser (Tree SyntaxTree.SyntaxUnit)
    funcDeclArg = do
      bracket B.Send B.Open
      argContent <- isIdTree B.Send <|> funcDecl B.Send
      bracket B.Send B.Close
      return argContent
      where
        isIdTree :: B.ScopeType -> Parser (Tree SyntaxTree.SyntaxUnit)
        isIdTree st = do
          idToTree <- isId
          return . Tree.tree . flip SyntaxTree.tokenUnitToSyntaxUnit st $ idToTree

sentence :: B.ScopeType -> Parser SyntaxTree.SyntaxTree
sentence st = expr st <|> statement st

program :: Parser (SyntaxTree.SyntaxTree)
program = do
  sentences <- some (sentence B.Return)
  return $
    (Tree.tree . SyntaxTree.genericSyntaxUnit) (Lexer.Data (D.Id "main")) -<= sentences

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------

nthChildMeetsCondition :: Int -> (SyntaxTree -> Bool) -> SyntaxTree -> Bool
nthChildMeetsCondition n f st
  | n < 0 = nthChildMeetsCondition ((length . Tree.treeChildren) st + n) f st
  | n > ((length . Tree.treeChildren) st - 1) = False
  | otherwise = (f . (!! n) . Tree.treeChildren) st

getSyntaxAttributeFromTree :: (SyntaxUnit -> a) -> SyntaxTree -> a
getSyntaxAttributeFromTree attr =
  Tree.maybeOnTreeNode ((attr . genericSyntaxUnit) (Lexer.Data D.Null)) attr

readTreeForError# ::
  (SyntaxTree -> Bool) ->
  (SyntaxTree -> SyntaxTree) ->
  SyntaxTree ->
  SyntaxTree
readTreeForError# _ _ Tree.Empty = Tree.Empty
readTreeForError# stopOn readF tr
  | stopOn tr =
    (Tree.reTree . readF) tr
      -<= map
        (readTreeForError# stopOn readF)
        (Tree.treeChildren tr)
  | otherwise =
    Tree.reTree tr
      -<= map
        (readTreeForError# stopOn readF)
        (Tree.treeChildren tr)
