{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TreeInterpreter.Environment
  ( SymbolPair (..),
    SymbolTable (..),
    SakanaRuntime (..),
    RuntimeEnvironment (..),
    emptyRuntimeEmptyTree,
    addSymbolToRuntime,
    addSymbolPairToTable,
    lookupBinding,
    maybeLookupBinding,
    makeNewRuntime,
    treeToSymbolPairIfNotAssigned,
    bindingNotFoundError,
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

newtype SymbolKey = SymbolKey {getSymbolKey :: String}
  deriving
    (Show, Read, Eq, GHC.Generics.Generic)

instance Data.Hashable.Hashable SymbolKey where
  hash (SymbolKey str) = Data.Hashable.hash str

data SymbolPair = SymbolPair
  { symbolId :: SakanaParser.SyntaxUnit,
    symbolVal :: SakanaParser.SyntaxTree
  }
  deriving (Show, Eq)

-- | An abstract type, used to transfer to a lamprey table
data LampreyBinding = LampreyBinding
  { lampreyBindingId :: SymbolKey,
    lampreyBindingVal :: Lamprey
  }
  deriving (Show, Eq)

data Lamprey = Lamprey
  { lampreyParams :: [SymbolKey],
    lampreyVal :: SakanaParser.SyntaxTree
  }
  deriving (Show, Eq)

data RuntimeEnvironment e v = RuntimeEnvironment
  { sakanaEnv :: e,
    sakanaVal :: v
  }

type SakanaRuntime a = RuntimeEnvironment SymbolTable a

-- | A RuntimeEnvironment capturing lambda calculus
type LampreyRuntime a = RuntimeEnvironment LampreyTable a

type SymbolTable = HashMap.HashMap SymbolKey SymbolPair

type LampreyTable = HashMap.HashMap SymbolKey Lamprey

----Lamprey functions---------------------------------------------------------------------
------------------------------------------------------------------------------------------
singletonLampreyTable :: LampreyBinding -> LampreyTable
singletonLampreyTable = CMonad.liftM2 HashMap.singleton lampreyBindingId lampreyBindingVal

treesToLamprey :: [SakanaParser.SyntaxTree] -> Lamprey
treesToLamprey =
  CMonad.liftM2
    Lamprey
    (map dataIdTreeToSymbolKey . filter Check.TreeIs.positionalArg)
    (sakanaVal . getExecutionTree)

addLampreyBindingToTable :: LampreyBinding -> LampreyTable -> LampreyTable
addLampreyBindingToTable =
  CMonad.liftM2 HashMap.insert (lampreyBindingId) (lampreyBindingVal)

-- | A lamprey is normal if and only if the top node of the lampreyVal is a data token
-- consisting of a primitive type, like Num, String, or Boolean.
-- Otherwise, it can be reduced through Beta Reduction
isNormal :: Lamprey -> Bool
isNormal =
  Tree.nodeStrictlySatisfies Check.NodeIs.dataTokenAndPrimitive . lampreyVal

extractFromNormal :: Lamprey -> D.Data
extractFromNormal l =
  if isNormal l then (getData l) else (raiseLampreyExtractionError l)
  where
    -- If this function is called in extractFromNormal,
    -- it is guaranteed to not be Nothing
    getData :: Lamprey -> D.Data
    getData =
      DMaybe.fromJust
        . (=<<) (SakanaParser.baseData . SakanaParser.token)
        . Tree.treeNode
        . lampreyVal
    raiseLampreyExtractionError :: (Show a) => a -> a2
    raiseLampreyExtractionError l' =
      Exception.raiseError $
        Exception.newException
          Exception.LampreyExtractionError
          []
          ("Attempting to extract value from non-normal lamprey: " ++ show l')
          Exception.Fatal

----Polymorphic Runtime Functions---------------------------------------------------------
------------------------------------------------------------------------------------------
runtimeUnit :: v1 -> RuntimeEnvironment (HashMap.HashMap k v2) v1
runtimeUnit = RuntimeEnvironment HashMap.empty

injectValueToRuntime :: v1 -> RuntimeEnvironment e v2 -> RuntimeEnvironment e v1
injectValueToRuntime x srt = srt {sakanaVal = x}

emptyRuntimeEmptyTree :: RuntimeEnvironment (HashMap.HashMap k v2) (Tree.Tree a)
emptyRuntimeEmptyTree = runtimeUnit Tree.Empty

getExecutionTree ::
  [SakanaParser.SyntaxTree] ->
  RuntimeEnvironment
    (HashMap.HashMap k v2)
    SakanaParser.SyntaxTree
getExecutionTree docTree =
  DMaybe.maybe
    ((runtimeUnit . getLastExecutionTree) docTree)
    runtimeUnit
    ((DList.find Check.TreeIs.swim) docTree)
  where
    getLastExecutionTree =
      DMaybe.fromMaybe Tree.Empty
        . Util.General.last'
        . filter (Check.TreeIs.executable)

dataIdTreeToSymbolKey :: SakanaParser.SyntaxTree -> SymbolKey
dataIdTreeToSymbolKey tr =
  ( SymbolKey
      . DMaybe.maybe
        ( Exception.raiseError
            ( Exception.newException
                Exception.General
                [SakanaParser.getSyntaxAttributeFromTree SakanaParser.line tr]
                ("Attempting to extract a hashable symbol from empty tree.")
                Exception.Fatal
            )
        )
        (SakanaParser.fromToken . SakanaParser.token)
      . Tree.treeNode
  )
    tr

----Runtime Binding Functions-------------------------------------------------------------
------------------------------------------------------------------------------------------

-- | Taking two runtimes, will union the two, with the first as the key.
-- Discard the second.
unionRuntime ::
  (Eq k, Data.Hashable.Hashable k) =>
  RuntimeEnvironment (HashMap.HashMap k v1) v2 ->
  RuntimeEnvironment (HashMap.HashMap k v1) v3 ->
  RuntimeEnvironment (HashMap.HashMap k v1) v2
unionRuntime sra srb = sra {sakanaEnv = HashMap.union (sakanaEnv sra) (sakanaEnv srb)}

-- | Taking a symbol table and runtime, unions the symbol table with the runtime
-- with the symbol table as the key.
-- return the updated runtime.
updateRuntime ::
  (Eq k, Data.Hashable.Hashable k) =>
  HashMap.HashMap k v1 ->
  RuntimeEnvironment (HashMap.HashMap k v1) v2 ->
  RuntimeEnvironment (HashMap.HashMap k v1) v2
updateRuntime st srt = srt {sakanaEnv = HashMap.union st (sakanaEnv srt)}

-- | A more generic version of the function, addSymbolToRuntime
addBindingToRuntime ::
  RuntimeEnvironment (HashMap.HashMap SymbolKey v1) v2 ->
  SymbolKey ->
  v1 ->
  RuntimeEnvironment (HashMap.HashMap SymbolKey v1) v2
addBindingToRuntime rt bid bval = updateRuntime (HashMap.singleton bid bval) rt

-- | reimplementation of lookupSymbolInEnvironmentStack
lookupBinding ::
  RuntimeEnvironment (HashMap.HashMap SymbolKey a) v2 ->
  SakanaParser.SyntaxUnit ->
  a
lookupBinding rte =
  CMonad.liftM2 DMaybe.fromMaybe (bindingNotFoundError) (maybeLookupBinding rte)

maybeLookupBinding ::
  RuntimeEnvironment (HashMap.HashMap SymbolKey v1) v2 ->
  SakanaParser.SyntaxUnit ->
  Maybe v1
maybeLookupBinding sr su = HashMap.lookup (getSymbolKeyFromSyntaxUnit su) (sakanaEnv sr)

bindingNotFoundError :: SakanaParser.SyntaxUnit -> a2
bindingNotFoundError lookupId =
  Exception.raiseError $
    Exception.newException
      Exception.SymbolNotFound
      [SakanaParser.line lookupId]
      ( "A value binding with the Id, \'"
          ++ ((SakanaParser.fromToken . SakanaParser.token) lookupId)
          ++ "\' does not exist in the current scope."
      )
      Exception.Fatal

----Runtime functions, as reimplemented EnvironmentStack functions------------------------
------------------------------------------------------------------------------------------

-- | Basically DEPRECATED
singletonSymbolTable :: SymbolPair -> SymbolTable
singletonSymbolTable sp = HashMap.insert (getKeyFromSymbolPair sp) sp HashMap.empty

-- | DEPRECATED
addSymbolToRuntime ::
  RuntimeEnvironment (HashMap.HashMap SymbolKey SymbolPair) v2 ->
  SymbolPair ->
  RuntimeEnvironment (HashMap.HashMap SymbolKey SymbolPair) v2
addSymbolToRuntime sr sp = updateRuntime (singletonSymbolTable sp) sr

-- | DEPRECATED
addSymbolPairToTable ::
  SymbolPair ->
  SymbolTable ->
  SymbolTable
addSymbolPairToTable sp = HashMap.insert (getKeyFromSymbolPair sp) sp

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
makeNewRuntime = flip RuntimeEnvironment Tree.Empty . makeSymbolTable

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

----Misc functions------------------------------------------------------------------------
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