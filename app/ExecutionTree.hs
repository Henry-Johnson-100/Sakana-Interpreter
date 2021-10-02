module ExecutionTree
  (
  )
where

import qualified Data.Maybe (fromJust)
import qualified Exception.Base as Exception
import qualified Lexer
import qualified SyntaxTree
import qualified Token.Bracket as B
import qualified Token.Data as D
import qualified Token.Keyword as K
import qualified Token.Operator as O
import qualified Token.Util.Like as LikeClass
import qualified Token.Util.Tree as Tree

s' =
  "fish add >(n)> >(m)> <(+ >(n)> >(m)> )<"
    ++ " fish sub >(n)> >(m)> <(- >(n)> >(m)> )< <(add >(1)> >(1)>)<"

t' = Lexer.tokenize s'

pt' = SyntaxTree.generateSyntaxTree t'

{-The most fundamental node execution is returning a primitive value
After that, performing a primitive operation like addition or subtraction
After that, calling a built-in function,
After that, defining a function,
After that, calling a defined function.-}

nodeIsDataToken :: SyntaxTree.SyntaxUnit -> Bool
nodeIsDataToken = LikeClass.like Lexer.genericData . SyntaxTree.token

getNodeTokenBaseData :: SyntaxTree.SyntaxTree -> D.Data
getNodeTokenBaseData =
  Tree.maybeOnTreeNode
    D.Null
    (Lexer.baseData . SyntaxTree.token)

nodeIsPrimitiveValue :: SyntaxTree.SyntaxTree -> Bool
nodeIsPrimitiveValue = D.isPrimitive . getNodeTokenBaseData

evaluatePrimitiveNode :: SyntaxTree.SyntaxTree -> D.Data
evaluatePrimitiveNode Tree.Empty = D.Null
evaluatePrimitiveNode tr
  | Tree.maybeOnTreeNode False nodeIsDataToken tr
      && (D.isPrimitive . getNodeTokenBaseData) tr =
    evaluatePrimitiveData tr

evaluatePrimitiveData :: SyntaxTree.SyntaxTree -> D.Data
evaluatePrimitiveData = getNodeTokenBaseData