{-# LANGUAGE DeriveGeneric #-}

module Interpreter.Environment
  ( BindingKey (..),
    Binding (..),
    SymbolTable (..),
    Runtime (..),
    insertBinding,
    singletonSymbolTable,
    newSymbolTable,
    lookup,
    maybeLookup,
    bindingExists,
    replaceSymbolTableStack,
    pushSymbolTable,
    popSymbolTable,
    replaceValue,
    replaceException,
    propagateException,
    transUnion,
    cisUnion,
    cleanRuntimeStack,
    throwJustError,
    runtimeBindingExists,
    runtimeMaybeLookup,
    runtimeLookup,
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
data BindingKey = BindingKey !String deriving (Show, Eq, GHC.Generics.Generic)

instance Data.Hashable.Hashable BindingKey where
  hash (BindingKey key) = Data.Hashable.hash key

instance UC.Format BindingKey where
  format (BindingKey key) = key

data Binding = Binding
  { bindingKey :: !BindingKey,
    bindingParams :: ![Syntax.SyntaxUnit],
    bindingTree :: !Syntax.SyntaxTree
  }
  deriving (Show, Eq)

instance UC.Defaultable Binding where
  defaultValue = Binding (BindingKey "") [] (UC.defaultValue)

instance UC.Format Binding where
  format (Binding bk params tree) =
    (UC.format bk)
      ++ ": "
      ++ (unwords . map UC.format) params
      ++ " ->\n"
      ++ (UC.format tree)

type SymbolTable = HashMap.HashMap BindingKey Syntax.SyntaxTree

instance (Eq k, Eq v) => UC.Defaultable (HashMap.HashMap k v) where
  defaultValue = HashMap.empty

instance (UC.Format k, UC.Format v) => UC.Format (HashMap.HashMap k v) where
  format = (unlines . map formatKVTuple . HashMap.toList)
    where
      formatKVTuple :: (UC.Format k, UC.Format v) => (k, v) -> String
      formatKVTuple (k, v) = UC.format k ++ ": " ++ UC.format v

data Runtime = Runtime
  { runtimeSymbolTableStack :: ![SymbolTable],
    runtimeValue :: ![Syntax.SyntaxTree],
    runtimeException :: !(Maybe.Maybe Exception.Exception)
  }
  deriving (Eq, Show)

instance UC.Defaultable Runtime where
  defaultValue = Runtime [] [] Maybe.Nothing

instance UC.Format Runtime where
  format rt =
    unlines
      [ "SYMBOL TABLE",
        (UC.format . runtimeSymbolTableStack) rt,
        "VALUE",
        (UC.format . runtimeValue) rt
      ]

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

newSymbolTable :: [Binding] -> SymbolTable
newSymbolTable [] = UC.defaultValue
newSymbolTable (b : bs) = List.foldl' insertBinding (singletonSymbolTable b) bs

lookup :: SymbolTable -> Syntax.SyntaxUnit -> Syntax.SyntaxTree
lookup st (Syntax.SyntaxUnit (Syntax.Data (Syntax.Id id)) line _) =
  Maybe.fromMaybe
    ((Exception.raiseError .< symbolNotFoundException) id line)
    (maybeLookup st id)
lookup _ su = (Exception.raiseError . improperLookupException) su

stackLookup :: [SymbolTable] -> Syntax.SyntaxUnit -> Syntax.SyntaxTree
stackLookup sts (Syntax.SyntaxUnit (Syntax.Data (Syntax.Id id)) line _) =
  Maybe.fromMaybe
    ((Exception.raiseError .< symbolNotFoundException) id line)
    (maybeStackLookup sts id)
stackLookup _ su = (Exception.raiseError . improperLookupException) su

maybeStackLookup :: [SymbolTable] -> String -> Maybe Syntax.SyntaxTree
maybeStackLookup (st : sts) str =
  case maybeLookup st str of
    Maybe.Nothing -> maybeStackLookup sts str
    result -> result
maybeStackLookup [] _ = Maybe.Nothing

symbolNotFoundException :: [Char] -> Int -> Exception.Exception
symbolNotFoundException id line =
  Exception.newException
    Exception.SymbolNotFound
    [line]
    ("Cannot find binding for ID: " ++ id)
    Exception.Fatal

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

stackBindingExists :: [SymbolTable] -> Syntax.SyntaxUnit -> Bool
stackBindingExists [] _ = False
stackBindingExists (st : sts) su =
  if bindingExists st su then True else stackBindingExists sts su

maybeLookup :: SymbolTable -> String -> Maybe.Maybe Syntax.SyntaxTree
maybeLookup st str = HashMap.lookup (BindingKey str) (st)

cleanStack :: [SymbolTable] -> [SymbolTable]
cleanStack = filter (not . UC.isDefault)

----Runtime Operations--------------------------------------------------------------------
------------------------------------------------------------------------------------------
replaceSymbolTableStack :: [SymbolTable] -> Runtime -> Runtime
replaceSymbolTableStack st rt = rt {runtimeSymbolTableStack = st}

pushSymbolTable :: SymbolTable -> Runtime -> Runtime
pushSymbolTable st (Runtime sts trs err) = Runtime (st : sts) trs err

-- | Returns the current top frame or an empty frame if none are present.
popSymbolTable :: Runtime -> SymbolTable
popSymbolTable (Runtime [] _ _) = UC.defaultValue
popSymbolTable (Runtime (st : sts) _ _) = st

replaceValue :: [Syntax.SyntaxTree] -> Runtime -> Runtime
replaceValue trs rt = rt {runtimeValue = trs}

replaceException :: Maybe.Maybe Exception.Exception -> Runtime -> Runtime
replaceException ex rt = rt {runtimeException = ex}

propagateException :: Runtime -> Runtime -> Runtime
propagateException rtex rt = replaceException (runtimeException rtex) rt

-- | Push the first runtime's stack to the head of the second.
--
-- Keep the second's value and exception.
transUnion :: Runtime -> Runtime -> Runtime
transUnion (Runtime stsa _ _) (Runtime stsb trsb errb) =
  Runtime (stsa ++ stsb) trsb errb

-- | Push the first runtime's stack to the head of the second.
--
-- Kepp the first's value and exception.
cisUnion :: Runtime -> Runtime -> Runtime
cisUnion (Runtime stsa trsa erra) (Runtime stsb _ _) =
  Runtime (stsa ++ stsb) trsa erra

-- | Removes empty stack frames from a Runtime.
cleanRuntimeStack :: Runtime -> Runtime
cleanRuntimeStack (Runtime sts trs err) = Runtime (cleanStack sts) trs err

throwJustError :: Runtime -> Runtime
throwJustError rt = Maybe.maybe rt Exception.raiseError (runtimeException rt)

runtimeBindingExists :: Runtime -> Syntax.SyntaxUnit -> Bool
runtimeBindingExists = stackBindingExists . runtimeSymbolTableStack

runtimeLookup :: Runtime -> Syntax.SyntaxUnit -> Syntax.SyntaxTree
runtimeLookup = stackLookup . runtimeSymbolTableStack

runtimeMaybeLookup :: Runtime -> String -> Maybe Syntax.SyntaxTree
runtimeMaybeLookup = maybeStackLookup . runtimeSymbolTableStack

-- injectBinding :: Binding -> Runtime -> Runtime
-- injectBinding = updateRuntimeSymbolTable . singletonSymbolTable

-- | Inject a binding into the top frame of the symbol table stack
-- Or create a new frame if no stack is present.
injectBinding :: Binding -> Runtime -> Runtime
injectBinding b (Runtime [] trs err) = Runtime [(singletonSymbolTable b)] trs err
injectBinding b (Runtime (st : sts) trs err) =
  Runtime ((flip (:) sts . insertBinding st) b) trs err