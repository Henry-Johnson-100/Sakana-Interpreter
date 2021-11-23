{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TreeInterpreter.Environment
  ( SymbolPair (..),
    SymbolTable (..),
    -- EnvironmentStack (..),
    SakanaRuntime (..),
    SakanaRuntimeT (..),
    -- emptyEnvironmentStack,
    emptyRuntimeEmptyTree,
    -- addSymbolToEnvironmentStack,
    addSymbolToRuntime,
    addSymbolPairToTable,
    -- lookupSymbolInEnvironmentStack,
    lookupSymbol,
    -- maybeLookupSymbolInEnvironmentStack,
    maybeLookupSymbol,
    -- makeEnvironmentStackFrame,
    makeNewRuntime,
    -- maybeTreeToSymbolPair,
    treeToSymbolPairIfNotAssigned,
    symbolNotFoundError,
    maybeLookupSymbolInSymbolTable,
    makeSymbolPair,
    unionRuntime,
    updateRuntime,
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

-- data EnvironmentStack = EnvironmentStack {envSymbolTable :: SymbolTable}
--   deriving (Show, Eq)

data SakanaRuntime a = SakanaRuntime
  { sakanaEnv :: SymbolTable,
    sakanaVal :: a
  }

instance Functor SakanaRuntime where
  fmap f ss = ss {sakanaVal = (f . sakanaVal) ss}

{-
The order of unions can be confusing since the first operand in <*>
is the key operand in union, but the second operand's resultant monad
in >>= is the key operand in union.

I believe these both behave in the same way since the function application
is applied in reverse direction for either instance:

In Applicative, the first is applied to the second,
in Monad, the second is applied to the first.

In either case, the key union operand is associated with the function
being applied and not the value it is being applied to.
-}

instance Applicative SakanaRuntime where
  pure = SakanaRuntime HashMap.empty
  (SakanaRuntime e1 f) <*> (SakanaRuntime e2 x) =
    SakanaRuntime (HashMap.union e1 e2) (f x)

instance Monad SakanaRuntime where
  return = pure
  (SakanaRuntime e1 x) >>= f = newRuntime {sakanaEnv = newEnv}
    where
      newRuntime = f x
      newEnv = HashMap.union (sakanaEnv newRuntime) e1

-- | A monad transformer using SakanaRuntime
newtype SakanaRuntimeT m a = SakanaRuntimeT {runSakanaRuntime :: m (SakanaRuntime a)}

instance Monad m => Functor (SakanaRuntimeT m) where
  fmap = CMonad.liftM

instance Monad m => Applicative (SakanaRuntimeT m) where
  pure = return
  (<*>) = CMonad.ap

-- This one was pretty frustrating to make desu, have to check the implementation
-- details later.
--
-- Seems kind of odd to me that you have to copy the implementation of a monad
-- for a transformer instead of being able to use that implementation programmatically
instance Monad m => Monad (SakanaRuntimeT m) where
  return = SakanaRuntimeT . return . SakanaRuntime HashMap.empty
  x >>= f = SakanaRuntimeT $ do
    t <- runSakanaRuntime x
    h <- runSakanaRuntime $ (f . sakanaVal) t
    return h {sakanaEnv = HashMap.union (sakanaEnv h) (sakanaEnv t)}

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

-- emptyEnvironmentStack :: EnvironmentStack
-- emptyEnvironmentStack = EnvironmentStack emptySymbolTable

----Runtime functions, as reimplemented EnvironmentStack functions------------------------
------------------------------------------------------------------------------------------

singletonSymbolTable :: SymbolPair -> SymbolTable
singletonSymbolTable sp = HashMap.insert (getKeyFromSymbolPair sp) sp HashMap.empty

-- | Taking two runtimes, will union the two, with the first as the key.
-- Discard the second.
unionRuntime :: SakanaRuntime a -> SakanaRuntime b -> SakanaRuntime a
unionRuntime sra = (=<<) (\x -> sra)

-- | Taking a symbol table and runtime, unions the symbol table with the runtime
-- with the symbol table as the key.
-- return the updated runtime.
updateRuntime :: SymbolTable -> SakanaRuntime a -> SakanaRuntime a
updateRuntime st = (<*>) (SakanaRuntime st id)

emptyRuntimeEmptyTree :: SakanaRuntime SakanaParser.SyntaxTree
emptyRuntimeEmptyTree = pure Tree.Empty

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
    -- (\x -> makeSymbolTable' (addSymbolPairToTable x st) trs)
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

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------

-- addSymbolToEnvironmentStack :: EnvironmentStack -> SymbolPair -> EnvironmentStack
-- addSymbolToEnvironmentStack env sb =
--   env {envSymbolTable = HashMap.insert (getKeyFromSymbolPair sb) sb (envSymbolTable env)}

-- lookupSymbolInEnvironmentStack ::
--   EnvironmentStack -> SakanaParser.SyntaxUnit -> SymbolPair
-- lookupSymbolInEnvironmentStack env lookupId =
--   DMaybe.fromMaybe
--     (symbolNotFoundError lookupId)
--     (maybeLookupSymbolInEnvironmentStack env lookupId)

-- maybeLookupSymbolInEnvironmentStack ::
--   EnvironmentStack -> SakanaParser.SyntaxUnit -> Maybe SymbolPair
-- maybeLookupSymbolInEnvironmentStack env su =
--   HashMap.lookup (getSymbolKeyFromSyntaxUnit su) (envSymbolTable env)

-- --these will require a little bit of effort to get working again
-- -------------------------------------
-- makeEnvironmentStackFrame :: [SakanaParser.SyntaxTree] -> EnvironmentStack
-- makeEnvironmentStackFrame = EnvironmentStack . makeSymbolTable

-- maybeTreeToSymbolPair :: SymbolTable -> SakanaParser.SyntaxTree -> Maybe SymbolPair
-- maybeTreeToSymbolPair st tr' =
--   if Check.TreeIs.storeable tr'
--     then (Just . checkForSameScopeAssignment st . makeSymbolPair) tr'
--     else Nothing

----------------------------------------
