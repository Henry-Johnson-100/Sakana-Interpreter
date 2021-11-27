{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TreeInterpreter.Environment
  ( SymbolPair (..),
    SymbolTable (..),
    SakanaRuntime (..),
    emptyRuntimeEmptyTree,
    addSymbolToRuntime,
    addSymbolPairToTable,
    lookupSymbol,
    maybeLookupSymbol,
    makeNewRuntime,
    treeToSymbolPairIfNotAssigned,
    symbolNotFoundError,
    maybeLookupSymbolInSymbolTable,
    makeSymbolPair,
    unionRuntime,
    updateRuntime,
    runtimeUnit,
  )
where

import qualified Control.Monad as CMonad
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

----data, newtype, type, instance---------------------------------------------------------
------------------------------------------------------------------------------------------

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

data SakanaRuntime a = SakanaRuntime
  { sakanaEnv :: SymbolTable,
    sakanaVal :: a
  }

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------

fPrintSymbolPair' :: SymbolPair -> String
fPrintSymbolPair' (SymbolPair sid tr) =
  concat ["Symbol ID: ", show sid, "\n", Tree.fPrintTree 0 tr]

getKeyFromSymbolPair :: SymbolPair -> SymbolKey
getKeyFromSymbolPair = getSymbolKeyFromSyntaxUnit . symbolId

getSymbolKeyFromSyntaxUnit :: SakanaParser.SyntaxUnit -> SymbolKey
getSymbolKeyFromSyntaxUnit = SymbolKey . SakanaParser.fromToken . SakanaParser.token

emptySymbolTable :: SymbolTable
emptySymbolTable = HashMap.empty

----Runtime functions, as reimplemented EnvironmentStack functions------------------------
------------------------------------------------------------------------------------------

singletonSymbolTable :: SymbolPair -> SymbolTable
singletonSymbolTable sp = HashMap.insert (getKeyFromSymbolPair sp) sp HashMap.empty

runtimeUnit :: a -> SakanaRuntime a
runtimeUnit = SakanaRuntime HashMap.empty

-- | Taking two runtimes, will union the two, with the first as the key.
-- Discard the second.
unionRuntime :: SakanaRuntime a -> SakanaRuntime b -> SakanaRuntime a
unionRuntime sra srb = sra {sakanaEnv = HashMap.union (sakanaEnv sra) (sakanaEnv srb)}

-- | Taking a symbol table and runtime, unions the symbol table with the runtime
-- with the symbol table as the key.
-- return the updated runtime.
updateRuntime :: SymbolTable -> SakanaRuntime a -> SakanaRuntime a
updateRuntime st srt = srt {sakanaEnv = HashMap.union st (sakanaEnv srt)}

injectValueToRuntime :: b -> SakanaRuntime a -> SakanaRuntime b
injectValueToRuntime x srt = srt {sakanaVal = x}

emptyRuntimeEmptyTree :: SakanaRuntime SakanaParser.SyntaxTree
emptyRuntimeEmptyTree = runtimeUnit Tree.Empty

addSymbolToRuntime :: SakanaRuntime a -> SymbolPair -> SakanaRuntime a
addSymbolToRuntime sr sp = updateRuntime (singletonSymbolTable sp) sr

addSymbolPairToTable ::
  SymbolPair ->
  SymbolTable ->
  SymbolTable
addSymbolPairToTable sp = HashMap.insert (getKeyFromSymbolPair sp) sp

-- | reimplementation of lookupSymbolInEnvironmentStack
lookupSymbol :: SakanaRuntime a -> SakanaParser.SyntaxUnit -> SymbolPair
lookupSymbol sr su = DMaybe.fromMaybe (symbolNotFoundError su) (maybeLookupSymbol sr su)

maybeLookupSymbol :: SakanaRuntime a -> SakanaParser.SyntaxUnit -> Maybe SymbolPair
maybeLookupSymbol sr su = HashMap.lookup (getSymbolKeyFromSyntaxUnit su) (sakanaEnv sr)

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

checkForSameScopeAssignment :: SymbolTable -> SymbolPair -> SymbolPair
checkForSameScopeAssignment st sp =
  DMaybe.maybe
    sp
    ( symbolAlreadyExistsException
        (symbolId sp)
        ((DMaybe.fromJust . flip maybeLookupSymbolInSymbolTable st . symbolId) sp)
    )
    (HashMap.lookup (getKeyFromSymbolPair sp) (st))

maybeLookupSymbolInSymbolTable ::
  SakanaParser.SyntaxUnit ->
  SymbolTable ->
  Maybe SymbolPair
maybeLookupSymbolInSymbolTable lookupId =
  DList.find (((SakanaParser.token) lookupId ==) . SakanaParser.token . symbolId)

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

-- | Reimplementation of maybeTreeToSymbolPair
treeToSymbolPairIfNotAssigned ::
  SakanaParser.SyntaxTree -> SymbolTable -> Maybe SymbolPair
treeToSymbolPairIfNotAssigned st sr =
  if Check.TreeIs.storeable st
    then (Just . checkForSameScopeAssignment (sr) . makeSymbolPair) st
    else Nothing

makeSymbolTable :: [SakanaParser.SyntaxTree] -> SymbolTable
makeSymbolTable = makeSymbolTable' HashMap.empty

makeSymbolTable' :: SymbolTable -> [SakanaParser.SyntaxTree] -> SymbolTable
makeSymbolTable' st [] = st
makeSymbolTable' st (tr : trs) =
  DMaybe.maybe
    (makeSymbolTable' st trs)
    (flip makeSymbolTable' trs . flip addSymbolPairToTable st)
    (treeToSymbolPairIfNotAssigned tr st)

makeNewRuntime :: [SakanaParser.SyntaxTree] -> SakanaRuntime SakanaParser.SyntaxTree
makeNewRuntime = flip SakanaRuntime Tree.Empty . makeSymbolTable

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
  | Check.TreeIs.symbolValueBinding tr =
    SymbolPair ((DMaybe.fromJust . Tree.treeNode) tr) tr
  where
    declId tr =
      DMaybe.fromMaybe
        (SakanaParser.genericSyntaxUnit (SakanaParser.Data D.Null))
        ((Util.General.head' . Tree.treeChildren) tr >>= Tree.treeNode)
