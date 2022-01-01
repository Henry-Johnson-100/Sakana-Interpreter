{-# LANGUAGE DeriveGeneric #-}

module Interpreter.Environment
  ( BindingKey (..),
    Binding (..),
    SymbolTable (..),
    Runtime (..),
    insertBinding,
    singletonSymbolTable,
    lookup,
    maybeLookup,
    bindingExists,
    replaceSymbolTable,
    replaceValue,
    replaceException,
    propagateException,
    transpropagateUnion,
    cisUnion,
    transUnion,
    throwJustError,
    runtimeBindingExists,
    runtimeMaybeLookup,
    runtimeLookup,
    updateRuntimeSymbolTable,
    injectBinding,
  )
where

import qualified Control.Monad as CMonad
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Hashable
import qualified Data.List as List hiding (lookup)
import qualified Data.Maybe as Maybe
import qualified Exception.Base as Exception
import qualified GHC.Generics
import qualified Parser.Syntax as Syntax
import qualified Util.Classes as UC
import Util.General ((.<))
import Prelude hiding (lookup)

----Data & Type & Instance Definitions----------------------------------------------------
------------------------------------------------------------------------------------------
data BindingKey = BindingKey String deriving (Show, Eq, GHC.Generics.Generic)

instance Data.Hashable.Hashable BindingKey where
  hash (BindingKey key) = Data.Hashable.hash key

instance UC.Format BindingKey where
  format (BindingKey key) = key

data Binding = Binding
  { bindingKey :: BindingKey,
    bindingTree :: Syntax.SyntaxTree
  }
  deriving (Show, Eq)

instance UC.Defaultable Binding where
  defaultValue = Binding (BindingKey "") (UC.defaultValue)

instance UC.Format Binding where
  format (Binding bk tree) = (UC.format bk) ++ ": " ++ (UC.format tree)

type SymbolTable = HashMap.HashMap BindingKey Syntax.SyntaxTree

instance (Eq k, Eq v) => UC.Defaultable (HashMap.HashMap k v) where
  defaultValue = HashMap.empty

instance (UC.Format k, UC.Format v) => UC.Format (HashMap.HashMap k v) where
  format = (unlines . map formatKVTuple . HashMap.toList)
    where
      formatKVTuple :: (UC.Format k, UC.Format v) => (k, v) -> String
      formatKVTuple (k, v) = UC.format k ++ ": " ++ UC.format v

data Runtime = Runtime
  { runtimeSymbolTable :: SymbolTable,
    runtimeValue :: [Syntax.SyntaxTree],
    runtimeException :: Maybe.Maybe Exception.Exception
  }
  deriving (Eq, Show)

instance UC.Defaultable Runtime where
  defaultValue = Runtime HashMap.empty [] Maybe.Nothing

instance UC.Format Runtime where
  format rt =
    ((UC.format . runtimeSymbolTable) rt)
      ++ "\n"
      ++ ((UC.format . runtimeValue) rt)

----General Operations--------------------------------------------------------------------
------------------------------------------------------------------------------------------
bindingTuple :: Binding -> (BindingKey, Syntax.SyntaxTree)
bindingTuple = CMonad.liftM2 (,) bindingKey bindingTree

----SymbolTable Operations----------------------------------------------------------------
------------------------------------------------------------------------------------------
insertBinding :: SymbolTable -> Binding -> SymbolTable
insertBinding st = flip (uncurry HashMap.insert) st . bindingTuple

singletonSymbolTable :: Binding -> SymbolTable
singletonSymbolTable = insertBinding HashMap.empty

lookup :: SymbolTable -> Syntax.SyntaxUnit -> Syntax.SyntaxTree
lookup st (Syntax.SyntaxUnit (Syntax.Data (Syntax.Id id)) line _) =
  Maybe.fromMaybe
    (Exception.raiseError symbolNotFoundException)
    (maybeLookup st id)
  where
    symbolNotFoundException =
      Exception.newException
        Exception.SymbolNotFound
        [line]
        ("Cannot find binding for ID: " ++ id)
        Exception.Fatal
lookup _ su = (Exception.raiseError . improperLookupException) su

improperLookupException :: Syntax.SyntaxUnit -> Exception.Exception
improperLookupException su =
  Exception.newException
    Exception.ImproperBindingLookup
    [Syntax.line su]
    ("Non-ID tokens cannot be bound to values: " ++ UC.format su)
    Exception.Fatal

bindingExists :: SymbolTable -> Syntax.SyntaxUnit -> Bool
bindingExists st su = case su of
  (Syntax.SyntaxUnit (Syntax.Data (Syntax.Id id)) _ _) ->
    (Maybe.isJust . maybeLookup st) id
  _ -> (Exception.raiseError . improperLookupException) su

maybeLookup :: SymbolTable -> String -> Maybe.Maybe Syntax.SyntaxTree
maybeLookup st str = HashMap.lookup (BindingKey str) (st)

----Runtime Operations--------------------------------------------------------------------
------------------------------------------------------------------------------------------
replaceSymbolTable :: SymbolTable -> Runtime -> Runtime
replaceSymbolTable st rt = rt {runtimeSymbolTable = st}

replaceValue :: [Syntax.SyntaxTree] -> Runtime -> Runtime
replaceValue trs rt = rt {runtimeValue = trs}

replaceException :: Exception.Exception -> Runtime -> Runtime
replaceException ex rt = rt {runtimeException = Maybe.Just ex}

propagateException :: Runtime -> Runtime -> Runtime
propagateException rtex rt =
  -- Could be a little more pointfree but I will keep it explicit for clarity.
  Maybe.maybe rt (flip replaceException rt) (runtimeException rtex)

{-
On unioning Runtime SymbolTables:
  4 permutations of Runtime unions where R is a runtime and T is a SymbolTable ->
    where Ta U Tb -> Tf (Tfst)
          Tb U Ta -> Ts (Tsnd)

            cis-flip(|)    transpropagation(/)
    Ra   Rb     Ra         Rb
       U    ->  |     X    |
    Ta   Tb     Tf         Ts
                           trans/cis-union product
For (R T)a<|>b as a or b:
So, cisUnion   a b -> Rb Ts
               b a -> Ra Tf (cis-flip)

    transUnion a b -> Ra Ts
               b a -> Rb Tf (trans-propagation)
-}

-- | Transpropagate a union of two Runtimes' SymbolTables, with the first as the master,
-- to the second Runtime.
--
-- defined as:
-- >flip transUnion
transpropagateUnion :: Runtime -> Runtime -> Runtime
transpropagateUnion = flip transUnion

-- | Replace a union of two Runtimes' SymbolTables, with the second as the master,
-- into the second Runtime.
cisUnion :: Runtime -> Runtime -> Runtime
cisUnion ra rb =
  let newTable = HashMap.union (runtimeSymbolTable rb) (runtimeSymbolTable ra)
   in replaceSymbolTable newTable rb

-- | Replace a union of two Runtimes' SymbolTables, with the second as the master,
-- into the first Runtime.
transUnion :: Runtime -> Runtime -> Runtime
transUnion ra rb =
  let newTable = HashMap.union (runtimeSymbolTable rb) (runtimeSymbolTable ra)
   in replaceSymbolTable newTable ra

throwJustError :: Runtime -> Runtime
throwJustError rt = Maybe.maybe rt Exception.raiseError (runtimeException rt)

runtimeBindingExists :: Runtime -> Syntax.SyntaxUnit -> Bool
runtimeBindingExists = bindingExists . runtimeSymbolTable

runtimeLookup :: Runtime -> Syntax.SyntaxUnit -> Syntax.SyntaxTree
runtimeLookup = lookup . runtimeSymbolTable

runtimeMaybeLookup :: Runtime -> String -> Maybe Syntax.SyntaxTree
runtimeMaybeLookup = maybeLookup . runtimeSymbolTable

-- | Union a SymbolTable with a Runtime's SymbolTable, with the former as the master,
-- and replace it into the Runtime.
updateRuntimeSymbolTable :: SymbolTable -> Runtime -> Runtime
updateRuntimeSymbolTable st rt =
  let newTable = HashMap.union st (runtimeSymbolTable rt)
   in replaceSymbolTable newTable rt

injectBinding :: Binding -> Runtime -> Runtime
injectBinding = updateRuntimeSymbolTable . singletonSymbolTable