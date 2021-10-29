module SakanaParser
  ( SyntaxTree (..),
    SyntaxUnit (..),
    generateSyntaxTreeMain,
    generateSyntaxTree,
    genericSyntaxUnit,
    getSyntaxAttributeFromTree,
    nthChildMeetsCondition,
    setContext,
    tokenUnitToSyntaxUnit,
  )
where

import qualified Data.Either as DEither (fromRight)
import qualified Data.Functor.Identity as DFId (Identity)
import qualified Data.List as DList (foldl')
import qualified Data.Maybe as DMaybe (maybe)
import Text.Parsec ((<|>))
import qualified Text.Parsec as Prs
  ( ParseError,
    ParsecT,
    SourceName,
    alphaNum,
    anyChar,
    char,
    count,
    digit,
    getPosition,
    many,
    many1,
    manyTill,
    notFollowedBy,
    oneOf,
    optionMaybe,
    parseTest,
    runParser,
    sourceLine,
    spaces,
    string,
    try,
  )
import qualified Token.Bracket as B (BracketTerminal (..), ScopeType (..))
import qualified Token.Control as C (Control (..))
import qualified Token.Data as D (Data (Id, Null, String), readData)
import qualified Token.Keyword as K (Keyword (Fish, Swim))
import qualified Token.Operator as O (Operator, readOp)
import Util.Tree ((-<=))
import qualified Util.Tree as Tree
  ( Tree (Empty),
    maybeOnTreeNode,
    tree,
    treeChildren,
  )

data Token
  = Bracket B.ScopeType B.BracketTerminal
  | Control C.Control
  | Data D.Data
  | Keyword K.Keyword
  | Operator O.Operator
  deriving (Show, Read, Eq)

data PacketUnit a = PacketUnit
  { unit :: a,
    unitLine :: Int
  }
  deriving (Show, Read, Eq)

data SyntaxUnit = SyntaxUnit
  { token :: Token,
    line :: Int,
    context :: B.ScopeType
  }
  deriving (Show, Eq)

type TokenUnit = PacketUnit Token

type SyntaxTree = Tree.Tree SyntaxUnit

type SakanaTokenParser u = Prs.ParsecT [Char] u DFId.Identity TokenUnit

type SakanaTreeParser u = Prs.ParsecT [Char] u DFId.Identity [SyntaxTree]

genericSyntaxUnit :: Token -> SyntaxUnit
genericSyntaxUnit t = SyntaxUnit t 0 B.Return

getSyntaxAttributeFromTree :: (SyntaxUnit -> b) -> SyntaxTree -> b
getSyntaxAttributeFromTree attr =
  Tree.maybeOnTreeNode ((attr . genericSyntaxUnit . Data) D.Null) (attr)

nthChildMeetsCondition :: Int -> (SyntaxTree -> Bool) -> SyntaxTree -> Bool
nthChildMeetsCondition n f st
  | n < 0 = nthChildMeetsCondition ((length . Tree.treeChildren) st + n) f st
  | n > ((length . Tree.treeChildren) st - 1) = False
  | otherwise = (f . (!! n) . Tree.treeChildren) st

setContext :: B.ScopeType -> SyntaxUnit -> SyntaxUnit
setContext st su = su {context = st}

tokenUnitToSyntaxUnit :: TokenUnit -> B.ScopeType -> SyntaxUnit
tokenUnitToSyntaxUnit tu = SyntaxUnit (unit tu) (unitLine tu)

dataDecimal :: Prs.ParsecT [Char] u DFId.Identity [Char]
dataDecimal = do
  dot <- Prs.char '.'
  decimal <- Prs.many1 Prs.digit
  return (dot : decimal)

dataDouble :: SakanaTokenParser u
dataDouble = do
  pos <- Prs.getPosition
  let ln = Prs.sourceLine pos
  negative <- Prs.optionMaybe (Prs.char '-')
  num <- (Prs.many1 Prs.digit)
  maybeDecimal <- Prs.optionMaybe dataDecimal
  let justNumStr =
        (DMaybe.maybe [] (\x -> id x : []) negative)
          ++ (DMaybe.maybe (num) (num ++) maybeDecimal)
  (return . flip PacketUnit ln . Data . D.readData) justNumStr

dataString :: SakanaTokenParser u
dataString = do
  pos <- Prs.getPosition
  let ln = Prs.sourceLine pos
  Prs.char '"'
  string <- Prs.manyTill Prs.anyChar (Prs.char '"')
  (return . flip PacketUnit ln . Data . D.String) string

dataBoolean :: SakanaTokenParser u
dataBoolean = do
  pos <- Prs.getPosition
  let ln = Prs.sourceLine pos
  b <- Prs.string "True" <|> Prs.string "False"
  (return . flip PacketUnit ln . Data . D.readData) b

dataNull :: SakanaTokenParser u
dataNull = do
  pos <- Prs.getPosition
  let ln = Prs.sourceLine pos
  (return . flip PacketUnit ln . Data) D.Null

dataData :: SakanaTokenParser u
dataData = Prs.try dataDouble <|> Prs.try dataString <|> Prs.try dataBoolean

identifier :: SakanaTokenParser u
identifier = do
  pos <- Prs.getPosition
  let ln = Prs.sourceLine pos
  idPre <- Prs.many1 validIdCharacter
  idPost <- Prs.many validIdPostId
  let combinedIdStr = idPre ++ (concat idPost)
  (return . flip PacketUnit ln . Data . D.Id) combinedIdStr
  where
    validIdCharacter :: Prs.ParsecT [Char] u DFId.Identity Char
    validIdCharacter = Prs.alphaNum <|> Prs.char '_' <|> Prs.char '\''
    validIdPostId = do
      dot <- Prs.count 1 (Prs.char '.')
      idPost <- Prs.many1 validIdCharacter
      return (dot ++ idPost)

operator :: SakanaTokenParser u
operator = do
  pos <- Prs.getPosition
  let ln = Prs.sourceLine pos
  opString <- singleCharOp <|> eq <|> nEqOrDiv <|> someThanOrEqual
  (return . flip PacketUnit ln . Operator . O.readOp) opString
  where
    singleCharOp :: Prs.ParsecT [Char] u DFId.Identity [Char]
    singleCharOp = do
      singleOp <- Prs.oneOf ['+', '-', '*', '^']
      return [singleOp]
    eq :: Prs.ParsecT [Char] u DFId.Identity [Char]
    eq = Prs.string "=="
    nEqOrDiv :: Prs.ParsecT [Char] u DFId.Identity [Char]
    nEqOrDiv = do
      div <- Prs.char '/'
      nEq <- Prs.optionMaybe (Prs.char '=')
      let nEqOrDiv' = div : DMaybe.maybe [] (: []) nEq
      return nEqOrDiv'
    someThanOrEqual :: Prs.ParsecT [Char] u DFId.Identity [Char]
    someThanOrEqual = do
      gtOrLt <- Prs.char '>' <|> Prs.char '<'
      Prs.notFollowedBy (Prs.char '(')
      eq <- Prs.optionMaybe (Prs.char '=')
      let someThanOrEqual' = gtOrLt : DMaybe.maybe [] (: []) eq
      return someThanOrEqual'

bracketSendOpen :: SakanaTokenParser u
bracketSendOpen = do
  pos <- Prs.getPosition
  let ln = Prs.sourceLine pos
  Prs.char '>'
  Prs.char '('
  (return . flip PacketUnit ln . Bracket B.Send) B.Open

bracketReturnOpen :: SakanaTokenParser u
bracketReturnOpen = do
  pos <- Prs.getPosition
  let ln = Prs.sourceLine pos
  Prs.char '<'
  Prs.char '('
  (return . flip PacketUnit ln . Bracket B.Return) B.Open

bracketSendClose :: SakanaTokenParser u
bracketSendClose = do
  pos <- Prs.getPosition
  let ln = Prs.sourceLine pos
  Prs.char ')'
  Prs.char '>'
  (return . flip PacketUnit ln . Bracket B.Send) B.Close

bracketReturnClose :: SakanaTokenParser u
bracketReturnClose = do
  pos <- Prs.getPosition
  let ln = Prs.sourceLine pos
  Prs.char ')'
  Prs.char '<'
  (return . flip PacketUnit ln . Bracket B.Return) B.Close

fish :: SakanaTokenParser u
fish = do
  pos <- Prs.getPosition
  let ln = Prs.sourceLine pos
  Prs.string "fish"
  Prs.notFollowedBy (Prs.alphaNum <|> Prs.char '.')
  return (PacketUnit (Keyword K.Fish) ln)

swim :: SakanaTokenParser u
swim = do
  pos <- Prs.getPosition
  let ln = Prs.sourceLine pos
  Prs.string "swim"
  Prs.notFollowedBy (Prs.alphaNum <|> Prs.char '.')
  return (PacketUnit (Keyword K.Swim) ln)

fin :: Prs.ParsecT [Char] u DFId.Identity TokenUnit
fin = do
  pos <- Prs.getPosition
  let ln = Prs.sourceLine pos
  Prs.string "fin"
  Prs.notFollowedBy (Prs.alphaNum <|> Prs.char '.')
  return (PacketUnit (Control C.Fin) (ln))

----Units to trees parsers----------------------------------------------------------------

foldAppendChildren :: Foldable t => Tree.Tree a -> t [Tree.Tree a] -> Tree.Tree a
foldAppendChildren toApp toFold = DList.foldl' (-<=) toApp toFold

maybeSpaced :: SakanaTreeParser u -> SakanaTreeParser u
maybeSpaced p = do
  p' <- p
  Prs.spaces
  return p'

tokenUnitToTree :: B.ScopeType -> TokenUnit -> SyntaxTree
tokenUnitToTree st = Tree.tree . flip tokenUnitToSyntaxUnit st

dataTree :: B.ScopeType -> SakanaTreeParser u
dataTree st = do
  dataToTree <- dataData
  Prs.spaces
  let dataTrees = [(tokenUnitToTree st) dataToTree]
  return dataTrees

idTree :: B.ScopeType -> SakanaTreeParser u
idTree st = do
  idToTree <- identifier
  Prs.spaces
  let idTrees = [tokenUnitToTree st idToTree]
  return idTrees

bracketContainingExpr :: B.ScopeType -> SakanaTreeParser u
bracketContainingExpr st = do
  if st == B.Send then bracketSendOpen else bracketReturnOpen
  Prs.spaces
  expr' <- expr B.Send
  Prs.spaces
  if st == B.Send then bracketSendClose else bracketReturnClose
  Prs.spaces
  return expr'

opExpr :: B.ScopeType -> SakanaTreeParser u
opExpr st = do
  opT <- operator
  Prs.spaces
  args <- (Prs.count 2 . bracketContainingExpr) B.Send
  Prs.spaces
  let opExprTrees =
        [((tokenUnitToTree st) opT `foldAppendChildren` args)]
  return opExprTrees

finExpr :: B.ScopeType -> SakanaTreeParser u
finExpr st = do
  f <- fin
  Prs.spaces
  args <- (Prs.count 3 . bracketContainingExpr) B.Send
  Prs.spaces
  let finTrees = [(tokenUnitToTree st) f `foldAppendChildren` args]
  return finTrees

fishSend :: SakanaTreeParser u
fishSend = do
  bracketSendOpen
  Prs.spaces
  bindTo <- identifier
  Prs.spaces
  bracketReturnOpen
  Prs.spaces
  valueToBind <- expr B.Return
  Prs.spaces
  bracketReturnClose
  Prs.spaces
  bracketSendClose
  Prs.spaces
  let sendTrees = [(tokenUnitToTree B.Send) bindTo -<= valueToBind]
  return sendTrees

swimExp :: B.ScopeType -> SakanaTreeParser u
swimExp st = do
  s <- swim
  Prs.spaces
  procs <- Prs.many (Prs.try (bracketContainingExpr B.Send) <|> fishSend)
  Prs.spaces
  returnValue <- bracketContainingExpr B.Return
  Prs.spaces
  let swimTrees = [(tokenUnitToTree st) s `foldAppendChildren` procs -<= returnValue]
  return swimTrees

funcCall :: B.ScopeType -> SakanaTreeParser u
funcCall st = do
  callId <- identifier
  Prs.spaces
  args <- Prs.many (bracketContainingExpr B.Send)
  Prs.spaces
  let funcCallTrees = [(tokenUnitToTree st) callId `foldAppendChildren` args]
  return funcCallTrees

funcDeclArg :: SakanaTreeParser u
funcDeclArg = do
  bracketSendOpen
  Prs.spaces
  argstmnt <- Prs.try (idTree B.Send) <|> funcDecl B.Send
  Prs.spaces
  bracketSendClose
  Prs.spaces
  return argstmnt

funcDecl :: B.ScopeType -> SakanaTreeParser u
funcDecl st = do
  f <- fish
  Prs.spaces
  declId <- identifier
  Prs.spaces
  declArgs <- Prs.many funcDeclArg
  Prs.spaces
  funcReturn <- Prs.try (bracketContainingExpr B.Return) <|> swimExp B.Return
  Prs.spaces
  let funcDeclTrees =
        [tokenUnitToTree st f -<= [(tokenUnitToTree B.Send) declId] `foldAppendChildren` declArgs -<= funcReturn]
  return funcDeclTrees

expr :: B.ScopeType -> SakanaTreeParser u
expr st =
  Prs.try (opExpr st)
    <|> Prs.try (finExpr st)
    <|> Prs.try (swimExp st)
    <|> Prs.try (funcCall st)
    <|> dataTree st

stmnt :: B.ScopeType -> SakanaTreeParser u
stmnt st = funcDecl st

program :: SakanaTreeParser u
program = do
  stmnts <- (Prs.many . maybeSpaced . stmnt) B.Return
  Prs.spaces
  toExecute <- (bracketContainingExpr B.Return) <|> swimExp B.Return
  let progTree =
        [ (tokenUnitToTree B.Return)
            (PacketUnit (Data (D.Id "main")) 0)
            `foldAppendChildren` stmnts -<= toExecute
        ]
  return progTree

runSakanaParser :: Prs.SourceName -> [Char] -> Either Prs.ParseError [SyntaxTree]
runSakanaParser srcName contents = do
  eitherDocTreeOrError <- Prs.runParser program () srcName contents
  return eitherDocTreeOrError

generateSyntaxTreeMain :: Prs.SourceName -> [Char] -> SyntaxTree
generateSyntaxTreeMain src = (head . DEither.fromRight [Tree.Empty] . runSakanaParser src)

generateSyntaxTree :: [Char] -> SyntaxTree
generateSyntaxTree str = head $ DEither.fromRight ([Tree.Empty]) (runSakanaParser "" str)

runSimpleParse = Prs.parseTest program