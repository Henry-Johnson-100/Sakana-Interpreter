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
    treeIsStoreable,
    treeIsSymbolValueBinding,
    nodeIsId,
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
import qualified Lexer
  ( Token (Data),
    dataTokenIsId,
    fromToken,
    keywordTokenIsDeclarationRequiringId,
  )
import qualified SyntaxTree
  ( SyntaxTree,
    SyntaxUnit (context, line, token),
    genericSyntaxUnit,
  )
import qualified Token.Bracket as B (ScopeType (Return))
import qualified Token.Data as D (Data (Null))
import qualified Util.General (head')
import qualified Util.Tree as Tree
  ( Tree (Empty),
    TreeIO (fPrintTree),
    nodeStrictlySatisfies,
    treeChildren,
    treeNode,
  )

data SymbolPair = SymbolPair
  { symbolId :: SyntaxTree.SyntaxUnit,
    symbolVal :: SyntaxTree.SyntaxTree
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

lookupSymbolInEnvironmentStack :: EnvironmentStack -> SyntaxTree.SyntaxUnit -> SymbolPair
lookupSymbolInEnvironmentStack env lookupId =
  DMaybe.fromMaybe
    (symbolNotFoundError lookupId)
    (maybeLookupSymbolInEnvironmentStack env lookupId)

maybeLookupSymbolInEnvironmentStack ::
  EnvironmentStack -> SyntaxTree.SyntaxUnit -> Maybe SymbolPair
maybeLookupSymbolInEnvironmentStack [] _ = Nothing
maybeLookupSymbolInEnvironmentStack (st : sts) lookupId =
  if DMaybe.isNothing maybeSymbolInCurrentTable
    then maybeLookupSymbolInEnvironmentStack sts lookupId
    else maybeSymbolInCurrentTable
  where
    maybeSymbolInCurrentTable = maybeLookupSymbolInSymbolTable lookupId st

makeEnvironmentStackFrame :: [SyntaxTree.SyntaxTree] -> EnvironmentStack
makeEnvironmentStackFrame = (: emptyEnvironmentStack) . makeSymbolTable

makeSymbolTable :: [SyntaxTree.SyntaxTree] -> SymbolTable
makeSymbolTable = makeSymbolTable' []

makeSymbolTable' :: SymbolTable -> [SyntaxTree.SyntaxTree] -> SymbolTable
makeSymbolTable' st [] = st
makeSymbolTable' st (tr : trs) =
  DMaybe.maybe
    (makeSymbolTable' st trs)
    (flip makeSymbolTable' trs . (: st))
    (maybeTreeToSymbolPair st tr)

maybeTreeToSymbolPair :: SymbolTable -> SyntaxTree.SyntaxTree -> Maybe SymbolPair
maybeTreeToSymbolPair st tr' =
  if treeIsStoreable tr'
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

symbolAlreadyExistsException :: SyntaxTree.SyntaxUnit -> SymbolPair -> a2
symbolAlreadyExistsException lookupId existingSymbol =
  Exception.raiseError $
    Exception.newException
      Exception.SymbolIsAlreadyBound
      [SyntaxTree.line lookupId, (SyntaxTree.line . symbolId) existingSymbol]
      ( "The symbol: \'"
          ++ (Lexer.fromToken . SyntaxTree.token) lookupId
          ++ "\' Already exists in the current scope and is bound to the symbol entry:\n"
          ++ fPrintSymbolPair' existingSymbol
      )
      Exception.Fatal

symbolNotFoundError :: SyntaxTree.SyntaxUnit -> a2
symbolNotFoundError lookupId =
  Exception.raiseError $
    Exception.newException
      Exception.SymbolNotFound
      [SyntaxTree.line lookupId]
      ( "A value binding with the Id, \'"
          ++ ((Lexer.fromToken . SyntaxTree.token) lookupId)
          ++ "\' does not exist in the current scope."
      )
      Exception.Fatal

maybeLookupSymbolInSymbolTable ::
  SyntaxTree.SyntaxUnit ->
  SymbolTable ->
  Maybe SymbolPair
maybeLookupSymbolInSymbolTable lookupId =
  DList.find (((SyntaxTree.token) lookupId ==) . SyntaxTree.token . symbolId)

-- | Take a syntax tree and create a symbol pair.
-- Is NOT agnostic, should only be called on trees where it would make sense to create a
-- symbol pair.
makeSymbolPair :: SyntaxTree.SyntaxTree -> SymbolPair
makeSymbolPair Tree.Empty =
  SymbolPair
    (SyntaxTree.genericSyntaxUnit (Lexer.Data D.Null))
    Tree.Empty
makeSymbolPair tr
  | Tree.nodeStrictlySatisfies
      nodeIsDeclarationRequiringId
      tr =
    SymbolPair (declId tr) tr
  | treeIsSymbolValueBinding tr = SymbolPair ((DMaybe.fromJust . Tree.treeNode) tr) tr
  where
    declId tr =
      DMaybe.fromMaybe
        (SyntaxTree.genericSyntaxUnit (Lexer.Data D.Null))
        ((Util.General.head' . Tree.treeChildren) tr >>= Tree.treeNode)

-- | Can be stored in a symbol table.
--  As of right now, treeIsStoreable and treeIsExecutable are not opposites.
--  because an anonymous function definition is not storeable
--  yet it is also not executable
--  but, named lambda functions: 'x <( >(m)> <(+ >(m)> >(1)>)<' for instance,
--  are storeable and should be stored as normal value bindings.
treeIsStoreable :: SyntaxTree.SyntaxTree -> Bool
treeIsStoreable = Tree.nodeStrictlySatisfies nodeIsDeclarationRequiringId

nodeIsDeclarationRequiringId :: SyntaxTree.SyntaxUnit -> Bool
nodeIsDeclarationRequiringId =
  Lexer.keywordTokenIsDeclarationRequiringId . SyntaxTree.token

-- | For fish code that looks like:
-- 'some_id <(***)<'
-- where '***' is some wildcard value
-- I would like to not have this as a feature in the language to be honest.
treeIsSymbolValueBinding :: SyntaxTree.SyntaxTree -> Bool
treeIsSymbolValueBinding tr =
  Tree.nodeStrictlySatisfies nodeIsId tr
    && firstChildIsReturnContext tr
  where
    firstChildIsReturnContext tr =
      case ((Util.General.head' . Tree.treeChildren) tr) >>= Tree.treeNode of
        Nothing -> False
        Just x -> ((B.Return ==) . SyntaxTree.context) x

nodeIsId :: SyntaxTree.SyntaxUnit -> Bool
nodeIsId = Lexer.dataTokenIsId . SyntaxTree.token