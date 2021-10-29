module TreeInterpreter.Environment
  ( SymbolPair (..),
    SymbolTable (..),
    EnvironmentStack (..),
    currentStackSymbolTable,
    enclosingEnvironmentStack,
    fPrintEnvironmentStack,
    fPrintSymbolPair',
    emptyEnvironmentStack,
    encloseEnvironmentIn,
    addSymbolToEnvironmentStack,
    addTableToEnvironmentStack,
    lookupSymbolInEnvironmentStack,
    maybeLookupSymbolInEnvironmentStack,
    makeEnvironmentStackFrame,
    makeSymbolTable,
    makeSymbolTable',
    maybeTreeToSymbolPair,
    symbolAlreadyExistsException,
    symbolNotFoundError,
    maybeLookupSymbolInSymbolTable,
    makeSymbolPair,
  )
where

import qualified Data.List as DList (find, intercalate)
import qualified Data.Maybe as DMaybe
  ( fromJust,
    fromMaybe,
    isNothing,
    maybe,
  )
import qualified Exception.Base as Exception
  ( ExceptionSeverity (Fatal),
    ExceptionType (SymbolIsAlreadyBound, SymbolNotFound),
    newException,
    raiseError,
  )
import qualified SakanaParser
  ( SyntaxTree,
    SyntaxUnit (line, token),
    Token (Data),
    fromToken,
    genericSyntaxUnit,
  )
import qualified Token.Bracket as B (ScopeType (Return))
import qualified Token.Data as D (Data (Null))
import qualified TreeInterpreter.LocalCheck.NodeIs as Check.NodeIs
  ( declarationRequiringId,
  )
import qualified TreeInterpreter.LocalCheck.TreeIs as Check.TreeIs
  ( storeable,
    symbolValueBinding,
  )
import qualified Util.General (head')
import qualified Util.Tree as Tree
  ( Tree (Empty),
    TreeIO (fPrintTree),
    nodeStrictlySatisfies,
    treeChildren,
    treeNode,
  )

data SymbolPair = SymbolPair
  { symbolId :: SakanaParser.SyntaxUnit,
    symbolVal :: SakanaParser.SyntaxTree
  }
  deriving (Show, Eq)

type SymbolTable = [SymbolPair]

type EnvironmentStack = [SymbolTable]

currentStackSymbolTable :: EnvironmentStack -> SymbolTable
currentStackSymbolTable [] = []
currentStackSymbolTable env = head env

enclosingEnvironmentStack :: EnvironmentStack -> EnvironmentStack
enclosingEnvironmentStack [] = []
enclosingEnvironmentStack (st : []) = []
enclosingEnvironmentStack (st : sts) = sts

fPrintEnvironmentStack :: EnvironmentStack -> [Char]
fPrintEnvironmentStack env =
  (DList.intercalate "\n" . map (DList.intercalate "\n" . map fPrintSymbolPair')) env

fPrintSymbolPair' :: SymbolPair -> String
fPrintSymbolPair' (SymbolPair sid tr) =
  concat ["Symbol ID: ", show sid, "\n", Tree.fPrintTree 0 tr]

emptyEnvironmentStack :: EnvironmentStack
emptyEnvironmentStack = []

-- | The first environment is prepended to the second,
-- meaning it is enclosed in the second.
encloseEnvironmentIn :: EnvironmentStack -> EnvironmentStack -> EnvironmentStack
encloseEnvironmentIn envInner envOuter = envInner ++ envOuter

addSymbolToEnvironmentStack :: EnvironmentStack -> SymbolPair -> EnvironmentStack
addSymbolToEnvironmentStack [] symPair = [[symPair]]
addSymbolToEnvironmentStack env symPair =
  (symPair : (currentStackSymbolTable env)) : (enclosingEnvironmentStack env)

addTableToEnvironmentStack :: EnvironmentStack -> SymbolTable -> EnvironmentStack
addTableToEnvironmentStack [] symTable = [symTable]
addTableToEnvironmentStack env symTable = symTable : env

lookupSymbolInEnvironmentStack :: EnvironmentStack -> SakanaParser.SyntaxUnit -> SymbolPair
lookupSymbolInEnvironmentStack env lookupId =
  DMaybe.fromMaybe
    (symbolNotFoundError lookupId)
    (maybeLookupSymbolInEnvironmentStack env lookupId)

maybeLookupSymbolInEnvironmentStack ::
  EnvironmentStack -> SakanaParser.SyntaxUnit -> Maybe SymbolPair
maybeLookupSymbolInEnvironmentStack [] _ = Nothing
maybeLookupSymbolInEnvironmentStack (st : sts) lookupId =
  if DMaybe.isNothing maybeSymbolInCurrentTable
    then maybeLookupSymbolInEnvironmentStack sts lookupId
    else maybeSymbolInCurrentTable
  where
    maybeSymbolInCurrentTable = maybeLookupSymbolInSymbolTable lookupId st

makeEnvironmentStackFrame :: [SakanaParser.SyntaxTree] -> EnvironmentStack
makeEnvironmentStackFrame = (: emptyEnvironmentStack) . makeSymbolTable

makeSymbolTable :: [SakanaParser.SyntaxTree] -> SymbolTable
makeSymbolTable = makeSymbolTable' []

makeSymbolTable' :: SymbolTable -> [SakanaParser.SyntaxTree] -> SymbolTable
makeSymbolTable' st [] = st
makeSymbolTable' st (tr : trs) =
  DMaybe.maybe
    (makeSymbolTable' st trs)
    (flip makeSymbolTable' trs . (: st))
    (maybeTreeToSymbolPair st tr)

maybeTreeToSymbolPair :: SymbolTable -> SakanaParser.SyntaxTree -> Maybe SymbolPair
maybeTreeToSymbolPair st tr' =
  if Check.TreeIs.storeable tr'
    then (Just . checkForSameScopeAssignment st . makeSymbolPair) tr'
    else Nothing

checkForSameScopeAssignment :: SymbolTable -> SymbolPair -> SymbolPair
checkForSameScopeAssignment [] sp = sp
checkForSameScopeAssignment st sp =
  if (DMaybe.isNothing . flip maybeLookupSymbolInSymbolTable st . symbolId) sp
    then sp
    else
      symbolAlreadyExistsException
        (symbolId sp)
        ((DMaybe.fromJust . flip maybeLookupSymbolInSymbolTable st . symbolId) sp)

symbolAlreadyExistsException :: SakanaParser.SyntaxUnit -> SymbolPair -> a2
symbolAlreadyExistsException lookupId existingSymbol =
  Exception.raiseError $
    Exception.newException
      Exception.SymbolIsAlreadyBound
      [SakanaParser.line lookupId, (SakanaParser.line . symbolId) existingSymbol]
      ( "The symbol: \'"
          ++ (SakanaParser.fromToken . SakanaParser.token) lookupId
          ++ "\' Already exists in the current scope and is bound to the symbol entry:\n"
          ++ fPrintSymbolPair' existingSymbol
      )
      Exception.Fatal

symbolNotFoundError :: SakanaParser.SyntaxUnit -> a2
symbolNotFoundError lookupId =
  Exception.raiseError $
    Exception.newException
      Exception.SymbolNotFound
      [SakanaParser.line lookupId]
      ( "A value binding with the Id, \'"
          ++ ((SakanaParser.fromToken . SakanaParser.token) lookupId)
          ++ "\' does not exist in the current scope."
      )
      Exception.Fatal

maybeLookupSymbolInSymbolTable ::
  SakanaParser.SyntaxUnit ->
  SymbolTable ->
  Maybe SymbolPair
maybeLookupSymbolInSymbolTable lookupId =
  DList.find (((SakanaParser.token) lookupId ==) . SakanaParser.token . symbolId)

-- | Take a syntax tree and create a symbol pair.
-- Is NOT agnostic, should only be called on trees where it would make sense to create a
-- symbol pair.
makeSymbolPair :: SakanaParser.SyntaxTree -> SymbolPair
makeSymbolPair Tree.Empty =
  SymbolPair
    (SakanaParser.genericSyntaxUnit (SakanaParser.Data D.Null))
    Tree.Empty
makeSymbolPair tr
  | Tree.nodeStrictlySatisfies
      Check.NodeIs.declarationRequiringId
      tr =
    SymbolPair (declId tr) tr
  | Check.TreeIs.symbolValueBinding tr = SymbolPair ((DMaybe.fromJust . Tree.treeNode) tr) tr
  where
    declId tr =
      DMaybe.fromMaybe
        (SakanaParser.genericSyntaxUnit (SakanaParser.Data D.Null))
        ((Util.General.head' . Tree.treeChildren) tr >>= Tree.treeNode)
