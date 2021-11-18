{-# LANGUAGE DeriveGeneric #-}

module TreeInterpreter.Environment
  ( Lamprey (..),
    LampreyBinding (..),
    LampreyClosure (..),
    LampreyKey (..),
    LampreyClosureT (..),
    Clownfish (..),
    LampreyTable (..),
    bindNoUnion,
    updateClosureTable,
    emptyTable,
    showLampreyBinding,
    showLampreyTable,
    insertLampreyBindingToTable,
    singletonLampreyTable,
  )
where

--previous export list
-- SymbolBinding (..),
-- SymbolTable (..),
-- EnvironmentStack (..),
-- LampreyKey (..),
-- currentStackSymbolTable,
-- enclosingEnvironmentStack,
-- fPrintEnvironmentStack,
-- fPrintSymbolPair',
-- emptyEnvironmentStack,
-- encloseEnvironmentIn,
-- insertSymbolToEnvironmentStack,
-- addTableToEnvironmentStack,
-- findSymbol,
-- maybeFindSymbol,
-- symbolAlreadyExistsException,
-- symbolNotFoundError,
-- checkForSameScopeAssignment,
-- functionTreeToSymbolPair,
-- maybeLookupStrInEnv,
-- maybeLookupKeyInEnv,
-- findExecutableChild,
-- getSymbolBindingKey,
-- getSyntaxUnitKey,
-- symbolInsert,
-- emptyTable,
-- makeSymbolBinding,

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Hashable as DHash
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

----Data Structures & Instances-----------------------------------------------------------
------------------------------------------------------------------------------------------

-- | A Piscine encoding of a lambda function as a SakanaParser.SyntaxTree.
-- Fields \'Params\' and \'Value\' are accessed with the \'lamprey\' prefix
data Lamprey = Lamprey
  { lampreyParams :: ![SakanaParser.SyntaxUnit],
    lampreyValue :: !SakanaParser.SyntaxTree
  }
  deriving (Show, Eq)

-- | A named Lamprey
-- Lamprey fields are accessed by the \'binding\' prefix
data LampreyBinding = LampreyBinding
  { bindingId :: !SakanaParser.SyntaxUnit,
    bindingLamprey :: !Lamprey
  }
  deriving (Show, Eq)

data LampreyClosure a = LampreyClosure
  { closureBindings :: !LampreyTable,
    closureValue :: !a
  }
  deriving (Show, Eq)

-- | A wrapper struct for a String, implementing Hashable.
-- To use in a LampreyTable.
newtype LampreyKey = LampreyKey {lampreyKey :: String}
  deriving (Eq, Show, GHC.Generics.Generic)

newtype LampreyClosureT m a = LampreyClosureT {runLampreyClosure :: m (LampreyClosure a)}

instance DHash.Hashable LampreyKey where
  hash (LampreyKey skr) = DHash.hash skr

instance Functor LampreyClosure where
  fmap f lc = lc {closureValue = (f . closureValue) lc}

-- | The LampreyClosure Applicative :: lc1 <*> lc2
-- Will apply the closureValue lc1 to the closureValue lc2
-- Additionally, the LampreyTable of lc1 will be unioned with the LampreyTable of lc2
-- such that, any keys shared in common between the two are overwritten by the values
-- found in lc1.
-- As such, Order of application is important as this operation is not commutative
-- in the event that two tables have the same keys.
instance Applicative LampreyClosure where
  pure = LampreyClosure emptyTable
  (LampreyClosure t1 f) <*> (LampreyClosure t2 x) =
    (fmap f . flip LampreyClosure x . HashMap.union t1) t2

-- | A monad, such that the new LampreyClosure resulting from the bind will have
-- a new LampreyTable as a union of the tables of mb and ma where any keys in common
-- will be overwritten by the values found in mb
instance Monad LampreyClosure where
  return = pure
  (LampreyClosure lt1 x) >>= f =
    let newClosure = f x
        newUnion = HashMap.union (closureBindings newClosure) lt1
     in newClosure {closureBindings = newUnion}

-- | Operates identically to >>= but there is no union of the LampreyTables,
-- only the new table is preserved.
-- This function violates the monadic laws of right identity and is not a properly
-- monadic function!
bindNoUnion :: LampreyClosure a -> (a -> LampreyClosure b) -> LampreyClosure b
(LampreyClosure lt1 x) `bindNoUnion` f = f x

type Clownfish = LampreyClosure Lamprey

type LampreyTable = HashMap.HashMap LampreyKey Lamprey

------------------------------------------------------------------------------------------
----Simple Functions on Structures--------------------------------------------------------
------------------------------------------------------------------------------------------

getLampreyBindingKey :: LampreyBinding -> LampreyKey
getLampreyBindingKey (LampreyBinding lid _) =
  (LampreyKey . SakanaParser.fromToken . SakanaParser.token) lid

identityClosure :: LampreyTable -> LampreyClosure (a -> a)
identityClosure = flip LampreyClosure id

updateClosureTable :: LampreyTable -> LampreyClosure a -> LampreyClosure a
updateClosureTable lt = (<*>) (identityClosure lt)

emptyTable :: LampreyTable
emptyTable = HashMap.empty

showLampreyBinding :: LampreyBinding -> String
showLampreyBinding lb =
  concat
    [ "ID: ",
      (lampreyKey . getLampreyBindingKey) lb,
      "\n",
      "Parameters: ",
      ( unwords
          . map
            (SakanaParser.fromToken . SakanaParser.token)
          . lampreyParams
          . bindingLamprey
      )
        lb,
      "\n",
      "Returns: ",
      Tree.fPrintTree 0 ((lampreyValue . bindingLamprey) lb)
    ]

showLampreyTable :: LampreyTable -> String
showLampreyTable = DList.intercalate "\n" . map showLampreyPair . HashMap.toList
  where
    showLampreyPair :: (LampreyKey, Lamprey) -> String
    showLampreyPair (LampreyKey k, l) =
      ( showLampreyBinding
          . LampreyBinding ((SakanaParser.genericSyntaxUnit . SakanaParser.Data . D.Id) k)
      )
        l

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------

insertLampreyBindingToTable :: LampreyBinding -> LampreyTable -> LampreyTable
insertLampreyBindingToTable lb =
  HashMap.insert (getLampreyBindingKey lb) (bindingLamprey lb)

singletonLampreyTable :: LampreyBinding -> LampreyTable
singletonLampreyTable = flip insertLampreyBindingToTable HashMap.empty

-- currentStackSymbolTable :: EnvironmentStack -> SymbolTable
-- currentStackSymbolTable [] =
--   Exception.raiseError $
--     Exception.newException
--       Exception.NoEnvironment
--       ([])
--       "There is no runtime environment!"
--       Exception.Fatal
-- currentStackSymbolTable env = head env

-- enclosingEnvironmentStack :: EnvironmentStack -> EnvironmentStack
-- enclosingEnvironmentStack (st : sts) = sts
-- enclosingEnvironmentStack _ = []

-- emptyEnvironmentStack :: EnvironmentStack
-- emptyEnvironmentStack = []

-- -- | The first environment is prepended to the second,
-- -- meaning it is enclosed in the second.
-- encloseEnvironmentIn :: EnvironmentStack -> EnvironmentStack -> EnvironmentStack
-- encloseEnvironmentIn envInner envOuter = envInner ++ envOuter

-- symbolInsert :: SymbolTable -> SymbolBinding -> SymbolTable
-- symbolInsert st = uncurry2 (\k v -> HashMap.insert k v st) . symbolKV

-- insertSymbolToEnvironmentStack :: EnvironmentStack -> SymbolBinding -> EnvironmentStack
-- insertSymbolToEnvironmentStack [] = listSingleton . uncurry2 HashMap.singleton . symbolKV
-- insertSymbolToEnvironmentStack (env : []) = listSingleton . symbolInsert env
-- insertSymbolToEnvironmentStack (env : envFrames) = (: envFrames) . symbolInsert env

-- listSingleton :: a -> [a]
-- listSingleton x = [x]

-- addTableToEnvironmentStack :: EnvironmentStack -> SymbolTable -> EnvironmentStack
-- addTableToEnvironmentStack [] symTable = [symTable]
-- addTableToEnvironmentStack env symTable = symTable : env

-- findSymbol ::
--   EnvironmentStack -> SakanaParser.SyntaxUnit -> SymbolBinding
-- findSymbol env lookupId =
--   DMaybe.fromMaybe
--     (symbolNotFoundError lookupId)
--     (maybeFindSymbol env lookupId)

-- maybeFindSymbol ::
--   EnvironmentStack -> SakanaParser.SyntaxUnit -> Maybe SymbolBinding
-- maybeFindSymbol [] _ = Nothing
-- maybeFindSymbol (st : sts) lookupId =
--   DMaybe.maybe
--     (maybeFindSymbol sts lookupId)
--     (Just)
--     (maybeSeeSymbol st lookupId)

-- maybeSeeSymbol :: SymbolTable -> SakanaParser.SyntaxUnit -> Maybe SymbolBinding
-- maybeSeeSymbol st = (HashMap.!?) st . getSyntaxUnitKey

-- maybeFindSymbolWithArgs ::
--   EnvironmentStack -> [SakanaParser.SyntaxUnit] -> Maybe SymbolBinding
-- maybeFindSymbolWithArgs env sb = maybeLookupKeyInEnv env (mapSBToKey sb)
--   where
--     mapSBToKey :: [SakanaParser.SyntaxUnit] -> LampreyKey
--     mapSBToKey = LampreyKey . concatMap (SakanaParser.fromToken . SakanaParser.token)

-- maybeLookupStrInEnv :: EnvironmentStack -> String -> Maybe SymbolBinding
-- maybeLookupStrInEnv env str = maybeLookupKeyInEnv env (LampreyKey str)

-- maybeLookupKeyInEnv :: EnvironmentStack -> LampreyKey -> Maybe SymbolBinding
-- maybeLookupKeyInEnv [] _ = Nothing
-- maybeLookupKeyInEnv (st : sts) k =
--   DMaybe.maybe
--     (maybeLookupKeyInEnv sts k)
--     (Just)
--     (HashMap.lookup k st)

-- checkForSameScopeAssignment :: SymbolTable -> SymbolBinding -> SymbolBinding
-- checkForSameScopeAssignment st sb =
--   DMaybe.maybe
--     sb
--     (symbolAlreadyExistsException (symbolId sb) sb)
--     (maybeSeeSymbol st (symbolId sb))

-- -- makeEnvironmentStackFrame :: [SakanaParser.SyntaxTree] -> EnvironmentStack
-- -- makeEnvironmentStackFrame = (: emptyEnvironmentStack) . makeSymbolTable

-- -- makeSymbolTable :: [SakanaParser.SyntaxTree] -> SymbolTable
-- -- makeSymbolTable = makeSymbolTable' []

-- -- makeSymbolTable' :: SymbolTable -> [SakanaParser.SyntaxTree] -> SymbolTable
-- -- makeSymbolTable' st [] = st
-- -- makeSymbolTable' st (tr : trs) =
-- --   DMaybe.maybe
-- --     (makeSymbolTable' st trs)
-- --     (flip makeSymbolTable' trs . (: st))
-- --     (maybeTreeToSymbolPair st tr)

-- -- maybeTreeToSymbolPair :: SymbolTable -> SakanaParser.SyntaxTree -> Maybe SymbolBinding
-- -- maybeTreeToSymbolPair st tr' =
-- --   if Check.TreeIs.storeable tr'
-- --     then (Just . checkForSameScopeAssignment st . functionTreeToSymbolPair) tr'
-- --     else Nothing

-- -- checkForSameScopeAssignment :: SymbolTable -> SymbolBinding -> SymbolBinding
-- -- checkForSameScopeAssignment [] sp = sp
-- -- checkForSameScopeAssignment st sp =
-- --   if (DMaybe.isNothing . flip seeSymbol st . symbolId) sp
-- --     then sp
-- --     else
-- --       symbolAlreadyExistsException
-- --         (symbolId sp)
-- --         ((DMaybe.fromJust . flip seeSymbol st . symbolId) sp)

-- -- seeSymbol ::
-- --   SakanaParser.SyntaxUnit ->
-- --   SymbolTable ->
-- --   Maybe SymbolBinding
-- -- seeSymbol lookupId = HashMap.lookup (getSyntaxUnitKey lookupId)

-- symbolAlreadyExistsException :: SakanaParser.SyntaxUnit -> SymbolBinding -> a2
-- symbolAlreadyExistsException lookupId existingSymbol =
--   Exception.raiseError $
--     Exception.newException
--       Exception.SymbolIsAlreadyBound
--       [SakanaParser.line lookupId, (SakanaParser.line . symbolId) existingSymbol]
--       ( "The symbol: \'"
--           ++ (SakanaParser.fromToken . SakanaParser.token) lookupId
--           ++ "\' Already exists in the current scope and is bound to the symbol entry:\n"
--           ++ fPrintSymbolPair' existingSymbol
--       )
--       Exception.Fatal

-- symbolNotFoundError :: SakanaParser.SyntaxUnit -> a2
-- symbolNotFoundError lookupId =
--   Exception.raiseError $
--     Exception.newException
--       Exception.SymbolNotFound
--       [SakanaParser.line lookupId]
--       ( "A value binding with the Id, \'"
--           ++ ((SakanaParser.fromToken . SakanaParser.token) lookupId)
--           ++ "\' does not exist in the current scope."
--       )
--       Exception.Fatal

-- uncurry2 :: (t1 -> t2 -> t3) -> (t1, t2) -> t3
-- uncurry2 f (a, b) = f a b

-- uncurry3 :: (t1 -> t2 -> t3 -> t4) -> (t1, t2, t3) -> t4
-- uncurry3 f (a, b, c) = f a b c

-- -- | Will bind a tree of shape 'function definition' to a new SymbolBinding
-- -- Accepts null or zero function parameters in the tree.
-- functionTreeToSymbolPair :: SakanaParser.SyntaxTree -> SymbolBinding
-- functionTreeToSymbolPair = uncurry3 SymbolBinding . functionTreeToReprOfSymbolPair

-- findExecutableChild ::
--   SakanaParser.SyntaxTree ->
--   SakanaParser.SyntaxTree
-- findExecutableChild tr =
--   DMaybe.maybe
--     (getLastExecutionTree tr)
--     id
--     ((DList.find Check.TreeIs.swim . Tree.treeChildren) tr)
--   where
--     getLastExecutionTree =
--       DMaybe.fromMaybe Tree.Empty
--         . Util.General.last'
--         . filter (Check.TreeIs.executable)
--         . Tree.treeChildren

-- functionTreeToReprOfSymbolPair :: SakanaParser.SyntaxTree -> ReprOfSymbolPair
-- functionTreeToReprOfSymbolPair tr =
--   let sId =
--         ( DMaybe.fromJust
--             . (=<<) Tree.treeNode
--             . Util.General.head'
--             . Tree.treeChildren
--         )
--           tr
--       params =
--         ( map (DMaybe.fromJust . Tree.treeNode)
--             . filter (not . Tree.maybeOnTreeNode True Check.NodeIs.nullNode)
--             . filter Check.TreeIs.positionalArg
--             . Tree.treeChildren
--         )
--           tr
--       val = (findExecutableChild) tr
--    in (sId, params, val)

-- -- | Called on storeable structures: function declarations or Anonymous functions.
-- -- If called on a function declaration (fish declaration), the provided SyntaxUnit
-- -- is ignored and a normal function declaration SymbolBinding is created.
-- -- If called on an anonymous function, a SymbolBinding is constructed from the provided
-- -- id, the send parameters of the anonymous function, and
-- -- the return tree of the anonymous function.
-- makeSymbolBinding ::
--   SakanaParser.SyntaxUnit -> [SakanaParser.SyntaxTree] -> SymbolBinding
-- makeSymbolBinding sId [] =
--   Exception.raiseError
--     ( Exception.generalException
--         "Error in makeSymbolBinding:\
--         \ No trees were passed to create a symbol"
--     )
-- makeSymbolBinding sId trs
--   | (Check.TreeIs.storeable . head) trs = (functionTreeToSymbolPair . head) trs
--   -- should hopefully smartly bind send context fishes and return context fishes
--   --as a function to the id sID
--   | otherwise =
--     SymbolBinding
--       sId
--       ( ( takeWhile ((==) B.Send . SakanaParser.context)
--             . map (DMaybe.fromJust . Tree.treeNode)
--         )
--           trs
--       )
--       ( DMaybe.fromMaybe
--           ( Exception.raiseError
--               ( Exception.generalException
--                   "Expected return expression but got Nothing\
--                   \ in Environment.makeSymbolBinding"
--               )
--           )
--           ( ( Util.General.head'
--                 . dropWhile
--                   ( (==) B.Return
--                       . SakanaParser.getSyntaxAttributeFromTree SakanaParser.context
--                   )
--             )
--               trs
--           )
--       )
