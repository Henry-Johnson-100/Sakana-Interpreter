{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TreeInterpreter.Environment
  ( Lamprey (..),
    LampreyBinding (..),
    RuntimeEnvironment (..),
    LampreyRuntime (..),
    addBindingToRuntime,
    injectValueToRuntime,
    nullLamprey,
    lampreyIsNormal,
    lampreyBindingFromSyntaxUnit,
    lampreyBindingFromString,
    extractFromLamprey,
    emptyRuntimeEmptyTree,
    lookupBinding,
    maybeLookupBinding,
    bindingNotFoundError,
    unionRuntime,
    updateRuntime,
    runtimeUnit,
    makeLampreyTable,
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
import qualified Token.Keyword as K
import qualified TreeInterpreter.LocalCheck.NodeIs as Check.NodeIs
import qualified TreeInterpreter.LocalCheck.TreeIs as Check.TreeIs
import qualified Util.Emptiable
import qualified Util.General
import qualified Util.Like
import qualified Util.Tree as Tree

----data, newtype, type, instance---------------------------------------------------------
------------------------------------------------------------------------------------------

newtype SymbolKey = SymbolKey {getSymbolKey :: String}
  deriving
    (Show, Read, Eq, GHC.Generics.Generic)

instance Data.Hashable.Hashable SymbolKey where
  hash (SymbolKey str) = Data.Hashable.hash str

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
  deriving (Eq, Show)

instance
  (Data.Hashable.Hashable k, Eq k, Eq v) =>
  Util.Emptiable.Emptiable (HashMap.HashMap k v)
  where
  empty = HashMap.empty

instance
  (Util.Emptiable.Emptiable e, Util.Emptiable.Emptiable v) =>
  Util.Emptiable.Emptiable (RuntimeEnvironment e v)
  where
  empty = RuntimeEnvironment Util.Emptiable.empty Util.Emptiable.empty

-- | A RuntimeEnvironment capturing lambda calculus
type LampreyRuntime a = RuntimeEnvironment LampreyTable a

type LampreyTable = HashMap.HashMap SymbolKey Lamprey

----Lamprey functions---------------------------------------------------------------------
------------------------------------------------------------------------------------------
nullLamprey :: Lamprey
nullLamprey =
  Lamprey [] . Tree.tree . SakanaParser.genericSyntaxUnit . SakanaParser.Data $ D.Null

singletonLampreyTable :: LampreyBinding -> LampreyTable
singletonLampreyTable = CMonad.liftM2 HashMap.singleton lampreyBindingId lampreyBindingVal

-- | Takes a list of trees as children from a Lamprey node and
-- returns a Lamprey.
--
-- Input structure is so =>
--
-- [[args :-<-: _]*, val :-<-: _]
treesToLamprey :: [SakanaParser.SyntaxTree] -> Lamprey
treesToLamprey =
  CMonad.liftM2
    Lamprey
    (map dataIdTreeToSymbolKey . filter Check.TreeIs.positionalArg)
    (sakanaVal . getExecutionTree)

-- | Main entry point for producing a LampreyTable from a document tree.
makeLampreyTable :: [SakanaParser.SyntaxTree] -> LampreyTable
makeLampreyTable = makeLampreyTable' HashMap.empty
  where
    makeLampreyTable' :: LampreyTable -> [SakanaParser.SyntaxTree] -> LampreyTable
    makeLampreyTable' lt [] = lt
    makeLampreyTable' lt (tr : trs) =
      DMaybe.maybe
        (makeLampreyTable' lt trs)
        (flip makeLampreyTable' trs . flip addLampreyBindingToTable lt)
        (treeToLampreyBindingIfNotAssigned lt tr)
    treeToLampreyBindingIfNotAssigned ::
      LampreyTable -> SakanaParser.SyntaxTree -> Maybe LampreyBinding
    treeToLampreyBindingIfNotAssigned lt tr =
      if isLampreyBindable tr
        then (DMaybe.Just . checkForLampreyAlreadyInScope lt . makeLampreyBinding) tr
        else Nothing
    checkForLampreyAlreadyInScope :: LampreyTable -> LampreyBinding -> LampreyBinding
    checkForLampreyAlreadyInScope lt lb =
      DMaybe.maybe
        lb
        ( CMonad.liftM2
            lampreyAlreadyExistsException
            lampreyBindingId
            (DMaybe.fromJust . flip HashMap.lookup lt . lampreyBindingId)
            lb
        )
        (HashMap.lookup (lampreyBindingId lb) lt)
    lampreyAlreadyExistsException :: SymbolKey -> Lamprey -> a2
    lampreyAlreadyExistsException lbid lb =
      Exception.raiseError $
        Exception.newException
          Exception.SymbolIsAlreadyBound
          []
          ""
          Exception.Fatal

-- | Used in treeToLampreyBindingIfNotAssigned
--
-- Should be kept up to date with the guards in makeLampreyBinding.
isLampreyBindable :: SakanaParser.SyntaxTree -> Bool
isLampreyBindable =
  Util.General.foldIdApplicativeOnSingleton
    any
    [Check.TreeIs.storeable, Check.TreeIs.fishSendBinding]

-- | Main entry point for making a generic LampreyBinding from a SyntaxTree
makeLampreyBinding :: SakanaParser.SyntaxTree -> LampreyBinding
makeLampreyBinding tr
  | Check.TreeIs.storeable tr = functionDeclTreeToLampreyBinding tr
  | Check.TreeIs.fishSendBinding tr = fishSendToLampreyBinding tr
  | otherwise =
    Exception.raiseError $
      Exception.newException
        Exception.LampreyBindingError
        ((map (SakanaParser.line) . Tree.flattenTree) tr)
        ( "Error creating lamprey binding in:\n"
            ++ Tree.fPrintTree 0 tr
            ++ "\nDoes not match guard cases in\
               \ TreeInterpreter.Environment.makeLampreyBinding"
        )
        Exception.Fatal

-- | For a fishSend declaration: '>(a <(5)<)>'
--
-- Will create a lamprey binding with appropriate binding id and lamprey
--
-- Note: Structure of fishSend tree is so =>
--
-- > IDNode -<- Lamprey -<= [[Args]?, Value]
-- where '?' denotes an optional value.
fishSendToLampreyBinding :: SakanaParser.SyntaxTree -> LampreyBinding
fishSendToLampreyBinding =
  CMonad.liftM2 lampreyBindingFromSyntaxUnit fishSendLamprey bindingId
  where
    bindingId :: SakanaParser.SyntaxTree -> SakanaParser.SyntaxUnit
    bindingId = DMaybe.fromJust . Tree.treeNode
    fishSendLamprey :: SakanaParser.SyntaxTree -> Lamprey
    fishSendLamprey =
      treesToLamprey . Tree.treeChildren . DMaybe.fromJust . Tree.firstChild

-- | For a function declaration like: 'fish add >(x)> >(y)> <(+ >(x)> >(y)> )<'
--
-- Will create a lamprey binding with the appropriate binding ID and lamprey.
--
-- - Note: the structure of the call is so =>
--
-- > Fish -<- IDNode -<- Lamprey -<= [[Args], Value]
functionDeclTreeToLampreyBinding :: SakanaParser.SyntaxTree -> LampreyBinding
functionDeclTreeToLampreyBinding tr =
  lampreyBindingFromSyntaxUnit (functionLamprey tr) (functionIdSyntaxUnit tr)
  where
    -- If the parent function is called, fromJust will never throw an error.
    functionIdSyntaxUnit :: SakanaParser.SyntaxTree -> SakanaParser.SyntaxUnit
    functionIdSyntaxUnit tr' = DMaybe.fromJust $ do
      idTree <- (Tree.firstChild) tr'
      idSyntaxUnit <- Tree.treeNode idTree
      DMaybe.Just idSyntaxUnit
    -- Do notation is just here for clarity and
    -- to avoid overly long function composition,
    -- it's not really necessary at all.
    functionLamprey :: SakanaParser.SyntaxTree -> Lamprey
    functionLamprey tr' = DMaybe.fromJust $ do
      idTree <- (Tree.firstChild) tr'
      lampreyTree <- (Tree.firstChild) tr'
      (DMaybe.Just . treesToLamprey . Tree.treeChildren) lampreyTree

-- | Create a LampreyBinding from a lamprey and any String.
--
-- A generic function that shouldn't really be used, as it is accessed by more specific
-- functions.
lampreyBindingFromString :: Lamprey -> String -> LampreyBinding
lampreyBindingFromString l = flip LampreyBinding l . SymbolKey

lampreyBindingFromSyntaxUnit :: Lamprey -> SakanaParser.SyntaxUnit -> LampreyBinding
lampreyBindingFromSyntaxUnit l su =
  ( lampreyBindingFromString l
      . D.fromData
      . DMaybe.maybe (bindingFromNotDataTokenError) (ifIsIdThenBindElseError)
      . SakanaParser.baseData
      . SakanaParser.token
  )
    su
  where
    bindingFromNotDataTokenError =
      Exception.raiseError $
        Exception.newException
          Exception.LampreyBindingError
          [SakanaParser.line su]
          ( "Cannot bind a lamprey to the token: "
              ++ (SakanaParser.fromToken . SakanaParser.token) su
          )
          Exception.Fatal
    ifIsIdThenBindElseError d =
      if Util.Like.like d (D.Id "") then d else bindingFromNotDataTokenError

addLampreyBindingToTable :: LampreyBinding -> LampreyTable -> LampreyTable
addLampreyBindingToTable =
  CMonad.liftM2 HashMap.insert (lampreyBindingId) (lampreyBindingVal)

-- | A lamprey is normal if and only if the top node of the lampreyVal is a data token
-- consisting of a primitive type, like Num, String, or Boolean.
--
-- Otherwise, it can be reduced through Beta Reduction
lampreyIsNormal :: Lamprey -> Bool
lampreyIsNormal =
  Tree.nodeStrictlySatisfies Check.NodeIs.dataTokenAndPrimitive . lampreyVal

extractFromLamprey :: Lamprey -> D.Data
extractFromLamprey l =
  if lampreyIsNormal l then (getData l) else (raiseLampreyExtractionError l)
  where
    -- If this function is called in extractFromLamprey,
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
runtimeUnit :: e -> RuntimeEnvironment (HashMap.HashMap k v) e
runtimeUnit = RuntimeEnvironment HashMap.empty

injectValueToRuntime :: v1 -> RuntimeEnvironment k v2 -> RuntimeEnvironment k v1
injectValueToRuntime x srt = srt {sakanaVal = x}

emptyRuntimeEmptyTree :: RuntimeEnvironment (HashMap.HashMap k v) (Tree.Tree a)
emptyRuntimeEmptyTree = runtimeUnit Tree.Empty

-- | Returns a runtime with an empty HashMap and suitable execution tree.
--
-- A suitable execution tree is either the first Swim tree or the last executable tree
-- according to Check.TreeIs.executable.
getExecutionTree ::
  [SakanaParser.SyntaxTree] ->
  RuntimeEnvironment
    (HashMap.HashMap k v)
    SakanaParser.SyntaxTree
getExecutionTree docTree =
  DMaybe.maybe
    ((runtimeUnit . getLastExecutionTree) docTree)
    runtimeUnit
    ((DList.find (Check.TreeIs.nodeEqualToKeyword K.Swim)) docTree)
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
  RuntimeEnvironment (HashMap.HashMap k v1) e1 ->
  RuntimeEnvironment (HashMap.HashMap k v1) e2 ->
  RuntimeEnvironment (HashMap.HashMap k v1) e1
unionRuntime sra srb = sra {sakanaEnv = HashMap.union (sakanaEnv sra) (sakanaEnv srb)}

-- | Taking a symbol table and runtime, unions the symbol table with the runtime
-- with the symbol table as the key.
-- return the updated runtime.
updateRuntime ::
  (Eq k, Data.Hashable.Hashable k) =>
  HashMap.HashMap k v1 ->
  RuntimeEnvironment (HashMap.HashMap k v1) e ->
  RuntimeEnvironment (HashMap.HashMap k v1) e
updateRuntime st srt = srt {sakanaEnv = HashMap.union st (sakanaEnv srt)}

-- | A more generic version of the function, addSymbolToRuntime
addBindingToRuntime ::
  RuntimeEnvironment (HashMap.HashMap SymbolKey v1) e ->
  SymbolKey ->
  v1 ->
  RuntimeEnvironment (HashMap.HashMap SymbolKey v1) e
addBindingToRuntime rt bid bval = updateRuntime (HashMap.singleton bid bval) rt

-- | reimplementation of lookupSymbolInEnvironmentStack
lookupBinding ::
  RuntimeEnvironment (HashMap.HashMap SymbolKey a) e ->
  SakanaParser.SyntaxUnit ->
  a
lookupBinding rte =
  CMonad.liftM2 DMaybe.fromMaybe (bindingNotFoundError) (maybeLookupBinding rte)

maybeLookupBinding ::
  RuntimeEnvironment (HashMap.HashMap SymbolKey v1) v2 ->
  SakanaParser.SyntaxUnit ->
  Maybe v1
maybeLookupBinding sr su =
  HashMap.lookup
    ((SymbolKey . SakanaParser.fromToken . SakanaParser.token) su)
    (sakanaEnv sr)

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
