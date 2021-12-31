module Interpreter.Inspection where

import qualified Data.Maybe as Maybe
import qualified Syntax
import qualified Util.General as UGen
import qualified Util.Tree as Tree

----Tree Checks---------------------------------------------------------------------------
------------------------------------------------------------------------------------------

onFirstElem :: (a -> Bool) -> [a] -> Bool
onFirstElem f = Maybe.maybe False f . UGen.head'

treeHeadIsPrimitiveData :: Syntax.SyntaxTree -> Bool
treeHeadIsPrimitiveData tr = case Tree.treeNode tr of
  (Maybe.Just (Syntax.SyntaxUnit (Syntax.Data d) _ _)) -> Syntax.isPrimitive d
  _ -> False

treeHeadIsPrimitiveNumType :: Syntax.SyntaxTree -> Bool
treeHeadIsPrimitiveNumType tr = case Tree.treeNode tr of
  (Maybe.Just (Syntax.SyntaxUnit (Syntax.Data (Syntax.Num n)) _ _)) -> True
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
