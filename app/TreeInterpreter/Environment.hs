{-# LANGUAGE DeriveGeneric #-}

module TreeInterpreter.Environment
  ( SymbolBinding (..),
    SymbolTable (..),
    EnvironmentStack (..),
    SymbolKey (..),
    currentStackSymbolTable,
    enclosingEnvironmentStack,
    fPrintEnvironmentStack,
    fPrintSymbolPair',
    emptyEnvironmentStack,
    encloseEnvironmentIn,
    insertSymbolToEnvironmentStack,
    addTableToEnvironmentStack,
    findSymbol,
    maybeFindSymbol,
    symbolAlreadyExistsException,
    symbolNotFoundError,
    checkForSameScopeAssignment,
    functionTreeToSymbolPair,
    maybeLookupStrInEnv,
    maybeLookupKeyInEnv,
    findExecutableChild,
    getSymbolBindingKey,
    getSyntaxUnitKey,
    symbolInsert,
    emptyTable,
  )
where

import qualified Data.HashMap.Strict as HashMap
  ( HashMap (..),
    elems,
    empty,
    insert,
    lookup,
    singleton,
    (!?),
  )
import qualified Data.Hashable as DHash (Hashable (hash))
import qualified Data.List as DList (find, intercalate)
import qualified Data.Maybe as DMaybe
  ( fromJust,
    fromMaybe,
    isNothing,
    maybe,
  )
import qualified Exception.Base as Exception
  ( ExceptionSeverity (Fatal),
    ExceptionType
      ( NoEnvironment,
        SymbolIsAlreadyBound,
        SymbolNotFound
      ),
    newException,
    raiseError,
  )
import qualified GHC.Generics (Generic)
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
    nullNode,
  )
import qualified TreeInterpreter.LocalCheck.TreeIs as Check.TreeIs
  ( executable,
    positionalArg,
    storeable,
    swim,
    symbolValueBinding,
  )
import qualified Util.General (head', last')
import qualified Util.Tree as Tree
  ( Tree (Empty),
    TreeIO (fPrintTree),
    maybeOnTreeNode,
    nodeStrictlySatisfies,
    treeChildren,
    treeNode,
  )

data SymbolKey = SymbolKey {symbolKeyRepresentation :: String}
  deriving (Eq, Show, GHC.Generics.Generic)

instance DHash.Hashable SymbolKey where
  hash (SymbolKey skr) = DHash.hash skr

data SymbolBinding = SymbolBinding
  { symbolId :: SakanaParser.SyntaxUnit,
    symbolParams :: [SakanaParser.SyntaxUnit],
    symbolVal :: SakanaParser.SyntaxTree
  }
  deriving (Show, Eq)

getSymbolBindingKey :: SymbolBinding -> SymbolKey
getSymbolBindingKey (SymbolBinding sbId _ _) =
  (SymbolKey . SakanaParser.fromToken . SakanaParser.token) sbId

getSyntaxUnitKey :: SakanaParser.SyntaxUnit -> SymbolKey
getSyntaxUnitKey = SymbolKey . SakanaParser.fromToken . SakanaParser.token

type ReprOfSymbolPair =
  ( SakanaParser.SyntaxUnit,
    [SakanaParser.SyntaxUnit],
    SakanaParser.SyntaxTree
  )

symbolKV :: SymbolBinding -> (SymbolKey, SymbolBinding)
symbolKV sb = (getSymbolBindingKey sb, sb)

type SymbolTable = HashMap.HashMap SymbolKey SymbolBinding

emptyTable :: HashMap.HashMap SymbolKey SymbolBinding
emptyTable = HashMap.empty

type EnvironmentStack = [SymbolTable]

fPrintEnvironmentStack :: EnvironmentStack -> [Char]
fPrintEnvironmentStack env =
  ( DList.intercalate "\n"
      . map (DList.intercalate "\n" . map (fPrintSymbolPair') . HashMap.elems)
  )
    env

fPrintSymbolPair' :: SymbolBinding -> String
fPrintSymbolPair' (SymbolBinding sid params tr) =
  concat
    [ "Symbol ID: ",
      show sid,
      "\nParams:\n",
      concatMap fPrintParam' params,
      Tree.fPrintTree 0 tr
    ]
  where
    fPrintParam' p = concat [show p, "\n"]

currentStackSymbolTable :: EnvironmentStack -> SymbolTable
currentStackSymbolTable [] =
  Exception.raiseError $
    Exception.newException
      Exception.NoEnvironment
      ([])
      "There is no runtime environment!"
      Exception.Fatal
currentStackSymbolTable env = head env

enclosingEnvironmentStack :: EnvironmentStack -> EnvironmentStack
enclosingEnvironmentStack (st : sts) = sts
enclosingEnvironmentStack _ = []

emptyEnvironmentStack :: EnvironmentStack
emptyEnvironmentStack = []

-- | The first environment is prepended to the second,
-- meaning it is enclosed in the second.
encloseEnvironmentIn :: EnvironmentStack -> EnvironmentStack -> EnvironmentStack
encloseEnvironmentIn envInner envOuter = envInner ++ envOuter

symbolInsert :: SymbolTable -> SymbolBinding -> SymbolTable
symbolInsert st = uncurry2 (\k v -> HashMap.insert k v st) . symbolKV

insertSymbolToEnvironmentStack :: EnvironmentStack -> SymbolBinding -> EnvironmentStack
insertSymbolToEnvironmentStack [] = listSingleton . uncurry2 HashMap.singleton . symbolKV
insertSymbolToEnvironmentStack (env : []) = listSingleton . symbolInsert env
insertSymbolToEnvironmentStack (env : envFrames) = (: envFrames) . symbolInsert env

listSingleton :: a -> [a]
listSingleton x = [x]

addTableToEnvironmentStack :: EnvironmentStack -> SymbolTable -> EnvironmentStack
addTableToEnvironmentStack [] symTable = [symTable]
addTableToEnvironmentStack env symTable = symTable : env

findSymbol ::
  EnvironmentStack -> SakanaParser.SyntaxUnit -> SymbolBinding
findSymbol env lookupId =
  DMaybe.fromMaybe
    (symbolNotFoundError lookupId)
    (maybeFindSymbol env lookupId)

maybeFindSymbol ::
  EnvironmentStack -> SakanaParser.SyntaxUnit -> Maybe SymbolBinding
maybeFindSymbol [] _ = Nothing
maybeFindSymbol (st : sts) lookupId =
  DMaybe.maybe
    (maybeFindSymbol sts lookupId)
    (Just)
    (maybeSeeSymbol st lookupId)

maybeSeeSymbol :: SymbolTable -> SakanaParser.SyntaxUnit -> Maybe SymbolBinding
maybeSeeSymbol st = (HashMap.!?) st . getSyntaxUnitKey

maybeFindSymbolWithArgs ::
  EnvironmentStack -> [SakanaParser.SyntaxUnit] -> Maybe SymbolBinding
maybeFindSymbolWithArgs env sb = maybeLookupKeyInEnv env (mapSBToKey sb)
  where
    mapSBToKey :: [SakanaParser.SyntaxUnit] -> SymbolKey
    mapSBToKey = SymbolKey . concatMap (SakanaParser.fromToken . SakanaParser.token)

maybeLookupStrInEnv :: EnvironmentStack -> String -> Maybe SymbolBinding
maybeLookupStrInEnv env str = maybeLookupKeyInEnv env (SymbolKey str)

maybeLookupKeyInEnv :: EnvironmentStack -> SymbolKey -> Maybe SymbolBinding
maybeLookupKeyInEnv [] _ = Nothing
maybeLookupKeyInEnv (st : sts) k =
  DMaybe.maybe
    (maybeLookupKeyInEnv sts k)
    (Just)
    (HashMap.lookup k st)

checkForSameScopeAssignment :: SymbolTable -> SymbolBinding -> SymbolBinding
checkForSameScopeAssignment st sb =
  DMaybe.maybe
    sb
    (symbolAlreadyExistsException (symbolId sb) sb)
    (maybeSeeSymbol st (symbolId sb))

-- makeEnvironmentStackFrame :: [SakanaParser.SyntaxTree] -> EnvironmentStack
-- makeEnvironmentStackFrame = (: emptyEnvironmentStack) . makeSymbolTable

-- makeSymbolTable :: [SakanaParser.SyntaxTree] -> SymbolTable
-- makeSymbolTable = makeSymbolTable' []

-- makeSymbolTable' :: SymbolTable -> [SakanaParser.SyntaxTree] -> SymbolTable
-- makeSymbolTable' st [] = st
-- makeSymbolTable' st (tr : trs) =
--   DMaybe.maybe
--     (makeSymbolTable' st trs)
--     (flip makeSymbolTable' trs . (: st))
--     (maybeTreeToSymbolPair st tr)

-- maybeTreeToSymbolPair :: SymbolTable -> SakanaParser.SyntaxTree -> Maybe SymbolBinding
-- maybeTreeToSymbolPair st tr' =
--   if Check.TreeIs.storeable tr'
--     then (Just . checkForSameScopeAssignment st . functionTreeToSymbolPair) tr'
--     else Nothing

-- checkForSameScopeAssignment :: SymbolTable -> SymbolBinding -> SymbolBinding
-- checkForSameScopeAssignment [] sp = sp
-- checkForSameScopeAssignment st sp =
--   if (DMaybe.isNothing . flip seeSymbol st . symbolId) sp
--     then sp
--     else
--       symbolAlreadyExistsException
--         (symbolId sp)
--         ((DMaybe.fromJust . flip seeSymbol st . symbolId) sp)

-- seeSymbol ::
--   SakanaParser.SyntaxUnit ->
--   SymbolTable ->
--   Maybe SymbolBinding
-- seeSymbol lookupId = HashMap.lookup (getSyntaxUnitKey lookupId)

symbolAlreadyExistsException :: SakanaParser.SyntaxUnit -> SymbolBinding -> a2
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

uncurry2 :: (t1 -> t2 -> t3) -> (t1, t2) -> t3
uncurry2 f (a, b) = f a b

uncurry3 :: (t1 -> t2 -> t3 -> t4) -> (t1, t2, t3) -> t4
uncurry3 f (a, b, c) = f a b c

-- | Will bind a tree of shape 'function definition' to a new SymbolBinding
-- Accepts null or zero function parameters in the tree.
functionTreeToSymbolPair :: SakanaParser.SyntaxTree -> SymbolBinding
functionTreeToSymbolPair = uncurry3 SymbolBinding . functionTreeToReprOfSymbolPair

findExecutableChild ::
  SakanaParser.SyntaxTree ->
  SakanaParser.SyntaxTree
findExecutableChild tr =
  DMaybe.maybe
    (getLastExecutionTree tr)
    id
    ((DList.find Check.TreeIs.swim . Tree.treeChildren) tr)
  where
    getLastExecutionTree =
      DMaybe.fromMaybe Tree.Empty
        . Util.General.last'
        . filter (Check.TreeIs.executable)
        . Tree.treeChildren

functionTreeToReprOfSymbolPair :: SakanaParser.SyntaxTree -> ReprOfSymbolPair
functionTreeToReprOfSymbolPair tr =
  let sId =
        ( DMaybe.fromJust
            . (=<<) Tree.treeNode
            . Util.General.head'
            . Tree.treeChildren
        )
          tr
      params =
        ( map (DMaybe.fromJust . Tree.treeNode)
            . filter (not . Tree.maybeOnTreeNode True Check.NodeIs.nullNode)
            . filter Check.TreeIs.positionalArg
            . Tree.treeChildren
        )
          tr
      val = (findExecutableChild) tr
   in (sId, params, val)
