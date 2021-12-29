module Interpreter
  (
  )
where

import qualified Control.Monad as CMonad
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List hiding (lookup)
import qualified Data.Maybe as Maybe
import qualified Exception.Base as Exception
import qualified Interpreter.Environment as Env
import qualified Syntax
import qualified System.IO as IO
import Util.General ((.<))
import qualified Util.General as UGen
import qualified Util.Tree as Tree
import Prelude hiding (lookup)

evaluateProgram :: Env.Runtime -> IO Syntax.SyntaxTree
evaluateProgram = fmap programOutputHead . interpret
  where
    programOutputHead :: Env.Runtime -> Syntax.SyntaxTree
    programOutputHead =
      Maybe.fromMaybe
        (Exception.raiseError programTerminatesAsNull)
        . (UGen.head' . Env.runtimeValue . Env.throwJustError)
    programTerminatesAsNull :: Exception.Exception
    programTerminatesAsNull =
      Exception.newException
        Exception.NullTree
        []
        "Null value at end of program."
        Exception.Fatal

interpret :: Env.Runtime -> IO Env.Runtime
interpret rt = interpret' . Env.throwJustError $ rt
  where
    interpret' :: Env.Runtime -> IO Env.Runtime
    -- Will cause an infinite hang if Env.throwJustError is not present in interpret.
    interpret' (Env.Runtime st [] err) =
      interpret
        ( Env.replaceException
            ( Exception.newException
                Exception.NullTree
                []
                "Unexpected null tree in the interpreter."
                Exception.Fatal
            )
            rt
        )
    -- Guards go here.
    -- trs could be null.
    interpret' (Env.Runtime st (tr : trs) err)
      | treeHeadIsPrimitiveData tr = return rt
      | treeHeadIsStandardLibraryCall tr = return rt
      | otherwise = return rt

-- | #TODO
evaluateFunction :: Env.Runtime -> IO Env.Runtime
evaluateFunction rt = evaluateFunction' rt
  where
    evaluateFunction' :: Env.Runtime -> IO Env.Runtime
    evaluateFunction' (Env.Runtime st (tr : trs) err)
      | Tree.nodeStrictlySatisfies nodeIsStandardLibCall tr = return rt
      | otherwise = return rt

-- | #TODO
evaluateStandardLibraryCall :: Env.Runtime -> IO Env.Runtime
evaluateStandardLibraryCall = return . id

----Tree Checks---------------------------------------------------------------------------
------------------------------------------------------------------------------------------

onFirstElem :: (a -> Bool) -> [a] -> Bool
onFirstElem f = Maybe.maybe False f . UGen.head'

treeHeadIsPrimitiveData :: Syntax.SyntaxTree -> Bool
treeHeadIsPrimitiveData tr = case Tree.treeNode tr of
  (Maybe.Just (Syntax.SyntaxUnit (Syntax.Data d) _ _)) -> Syntax.isPrimitive d
  _ -> False

treeHasNoChildren :: Tree.Tree a -> Bool
treeHasNoChildren = null . Tree.treeChildren

treeHasNoReturnChildren :: Syntax.SyntaxTree -> Bool
treeHasNoReturnChildren =
  null
    . filter (Tree.nodeStrictlySatisfies nodeIsReturnContext)
    . Tree.treeChildren

treeHeadIsId :: Syntax.SyntaxTree -> Bool
treeHeadIsId = Tree.nodeStrictlySatisfies nodeIsId

treeHeadIsFunctionCall :: Syntax.SyntaxTree -> Bool
treeHeadIsFunctionCall =
  UGen.foldIdApplicativeOnSingleton all [treeHeadIsId, treeHasNoReturnChildren]

treeHeadIsPositionalParameter :: Syntax.SyntaxTree -> Bool
treeHeadIsPositionalParameter =
  UGen.foldIdApplicativeOnSingleton all [treeHeadIsId, treeHasNoChildren]

treeHeadIsStandardLibraryCall :: Syntax.SyntaxTree -> Bool
treeHeadIsStandardLibraryCall =
  UGen.foldIdApplicativeOnSingleton
    all
    [treeHeadIsFunctionCall, Tree.nodeStrictlySatisfies nodeIsStandardLibCall]

----Node Checks---------------------------------------------------------------------------
------------------------------------------------------------------------------------------

nodeIsId :: Syntax.SyntaxUnit -> Bool
nodeIsId su = case Syntax.token su of
  (Syntax.Data (Syntax.Id _)) -> True
  _ -> False

nodeIsReturnContext :: Syntax.SyntaxUnit -> Bool
nodeIsReturnContext su = case Syntax.context su of
  Syntax.Return -> True
  _ -> False

nodeIsSendContext :: Syntax.SyntaxUnit -> Bool
nodeIsSendContext = not . nodeIsReturnContext

nodeIsStandardLibCall :: Syntax.SyntaxUnit -> Bool
nodeIsStandardLibCall su = case Syntax.token su of
  (Syntax.Data (Syntax.Id id)) -> elem id standardLibIdStrings
  _ -> False

----Traverse and Retrieve Functions-------------------------------------------------------
------------------------------------------------------------------------------------------

-- | Retrieve the positional parameters from a syntax tree whose parent node is a Lamprey.
--
-- Should work, but since it only checks the parent nodes of the children of Lamprey,
-- there may be some circumstances, though I don't foresee any, where it may yield some
-- unexpected results.
getLampreyPositionalParameters :: Syntax.SyntaxTree -> [Syntax.SyntaxUnit]
getLampreyPositionalParameters =
  UGen.filterFromJust
    (UGen.foldIdApplicativeOnSingleton all [nodeIsId, nodeIsSendContext])
    . map Tree.treeNode
    . Tree.treeChildren

-- | Retrieve the value (Return context tree) of a tree whose parent node is a Lamprey.
--
-- This function uses the partial function 'last' but it is guaranteed to work due to
-- the initial pass through the parser. That is, all Lamprey trees are guaranteed to have
-- one and only one Return child.
getLampreyValue :: Syntax.SyntaxTree -> Syntax.SyntaxTree
getLampreyValue = last . Tree.treeChildren

----Inline Standard Library---------------------------------------------------------------
------------------------------------------------------------------------------------------
standardLibIdStrings :: [String]
standardLibIdStrings =
  [ "+",
    "-",
    "/",
    "*",
    "^",
    "==",
    "/=",
    ">",
    "<",
    ">=",
    "<=",
    "fin",
    "#INLINE#",
    "to_string",
    "to_bool",
    "concat_str",
    "read_prim",
    "floor"
  ]

getSakanaArgForInlineBody :: String
getSakanaArgForInlineBody = ""

getSakanaNumDraft :: Syntax.Data -> Double
getSakanaNumDraft (Syntax.Num n) = n
getSakanaNumDraft _ = throwGeneralError

getSakanaArgDraft :: Syntax.SyntaxTree -> Syntax.Data
getSakanaArgDraft tr = case Tree.treeNode tr of
  (Maybe.Just (Syntax.SyntaxUnit (Syntax.Data d) _ _)) -> d
  _ -> throwGeneralError

throwGeneralError :: a
throwGeneralError =
  Exception.raiseError
    (Exception.newException Exception.General [] "" Exception.Fatal)