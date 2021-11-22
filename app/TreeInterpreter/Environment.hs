{-# LANGUAGE DeriveGeneric #-}

module TreeInterpreter.Environment
  ( SymbolPair (..),
    SymbolTable (..),
    EnvironmentStack (..),
    -- currentStackSymbolTable,
    -- enclosingEnvironmentStack,
    -- fPrintEnvironmentStack,
    -- fPrintSymbolPair',
    emptyEnvironmentStack,
    -- encloseEnvironmentIn,
    addSymbolToEnvironmentStack,
    addSymbolPairToTable,
    -- addTableToEnvironmentStack,
    lookupSymbolInEnvironmentStack,
    maybeLookupSymbolInEnvironmentStack,
    makeEnvironmentStackFrame,
    -- makeSymbolTable,
    -- makeSymbolTable',
    maybeTreeToSymbolPair,
    -- symbolAlreadyExistsException,
    symbolNotFoundError,
    maybeLookupSymbolInSymbolTable,
    makeSymbolPair,
  )
where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Hashable
import qualified Data.List as DList
import qualified Data.Maybe as DMaybe
import qualified Exception.Base as Exception
import qualified GHC.Generics
import qualified SakanaParser
import qualified Token.Bracket as B
import qualified Token.Data as D
import qualified TreeInterpreter.LocalCheck.NodeIs as Check.NodeIs
import qualified TreeInterpreter.LocalCheck.TreeIs as Check.TreeIs
import qualified Util.General
import qualified Util.Tree as Tree

data SymbolPair = SymbolPair
  { symbolId :: SakanaParser.SyntaxUnit,
    symbolVal :: SakanaParser.SyntaxTree
  }
  deriving (Show, Eq)

newtype SymbolKey = SymbolKey {getSymbolKey :: String}
  deriving
    (Show, Read, Eq, GHC.Generics.Generic)

instance Data.Hashable.Hashable SymbolKey where
  hash (SymbolKey str) = Data.Hashable.hash str

type SymbolTable = HashMap.HashMap SymbolKey SymbolPair

data EnvironmentStack = EnvironmentStack {envSymbolTable :: SymbolTable} deriving (Show, Eq)

-- currentStackSymbolTable :: EnvironmentStack -> SymbolTable
-- currentStackSymbolTable [] = []
-- currentStackSymbolTable env = head env

-- enclosingEnvironmentStack :: EnvironmentStack -> EnvironmentStack
-- enclosingEnvironmentStack [] = []
-- enclosingEnvironmentStack (st : []) = []
-- enclosingEnvironmentStack (st : sts) = sts

-- fPrintEnvironmentStack :: EnvironmentStack -> [Char]
-- fPrintEnvironmentStack env =
--   (DList.intercalate "\n" . map (DList.intercalate "\n" . map fPrintSymbolPair')) env

fPrintSymbolPair' :: SymbolPair -> String
fPrintSymbolPair' (SymbolPair sid tr) =
  concat ["Symbol ID: ", show sid, "\n", Tree.fPrintTree 0 tr]

getKeyFromSymbolPair :: SymbolPair -> SymbolKey
getKeyFromSymbolPair = getSymbolKeyFromSyntaxUnit . symbolId

getSymbolKeyFromSyntaxUnit = SymbolKey . SakanaParser.fromToken . SakanaParser.token

emptySymbolTable :: SymbolTable
emptySymbolTable = HashMap.empty

emptyEnvironmentStack :: EnvironmentStack
emptyEnvironmentStack = EnvironmentStack emptySymbolTable

-- -- | The first environment is prepended to the second,
-- -- meaning it is enclosed in the second.
-- encloseEnvironmentIn :: EnvironmentStack -> EnvironmentStack -> EnvironmentStack
-- encloseEnvironmentIn envInner envOuter = envInner ++ envOuter

addSymbolToEnvironmentStack :: EnvironmentStack -> SymbolPair -> EnvironmentStack
addSymbolToEnvironmentStack env sb =
  env {envSymbolTable = HashMap.insert (getKeyFromSymbolPair sb) sb (envSymbolTable env)}

addSymbolPairToTable ::
  SymbolPair ->
  SymbolTable ->
  SymbolTable
addSymbolPairToTable sp = HashMap.insert (getKeyFromSymbolPair sp) sp

-- addTableToEnvironmentStack :: EnvironmentStack -> SymbolTable -> EnvironmentStack
-- addTableToEnvironmentStack [] symTable = [symTable]
-- addTableToEnvironmentStack env symTable = symTable : env

lookupSymbolInEnvironmentStack :: EnvironmentStack -> SakanaParser.SyntaxUnit -> SymbolPair
lookupSymbolInEnvironmentStack env lookupId =
  DMaybe.fromMaybe
    (symbolNotFoundError lookupId)
    (maybeLookupSymbolInEnvironmentStack env lookupId)

maybeLookupSymbolInEnvironmentStack ::
  EnvironmentStack -> SakanaParser.SyntaxUnit -> Maybe SymbolPair
maybeLookupSymbolInEnvironmentStack env su =
  HashMap.lookup (getSymbolKeyFromSyntaxUnit su) (envSymbolTable env)

--these will require a little bit of effort to get working again
------------------------------------------------------------------------------------------
makeEnvironmentStackFrame :: [SakanaParser.SyntaxTree] -> EnvironmentStack
makeEnvironmentStackFrame = EnvironmentStack . makeSymbolTable

makeSymbolTable :: [SakanaParser.SyntaxTree] -> SymbolTable
makeSymbolTable = makeSymbolTable' HashMap.empty

makeSymbolTable' :: SymbolTable -> [SakanaParser.SyntaxTree] -> SymbolTable
makeSymbolTable' st [] = st
makeSymbolTable' st (tr : trs) =
  DMaybe.maybe
    (makeSymbolTable' st trs)
    -- (\x -> makeSymbolTable' (addSymbolPairToTable x st) trs)
    (flip makeSymbolTable' trs . flip addSymbolPairToTable st)
    (maybeTreeToSymbolPair st tr)

maybeTreeToSymbolPair :: SymbolTable -> SakanaParser.SyntaxTree -> Maybe SymbolPair
maybeTreeToSymbolPair st tr' =
  if Check.TreeIs.storeable tr'
    then (Just . checkForSameScopeAssignment st . makeSymbolPair) tr'
    else Nothing

checkForSameScopeAssignment :: SymbolTable -> SymbolPair -> SymbolPair
checkForSameScopeAssignment st sp =
  DMaybe.maybe
    sp
    (symbolAlreadyExistsException (symbolId sp) ((DMaybe.fromJust . flip maybeLookupSymbolInSymbolTable st . symbolId) sp))
    (HashMap.lookup (getKeyFromSymbolPair sp) (st))

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

------------------------------------------------------------------------------------------

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
