module ExecutionTree
  (
  )
where

import qualified Data.Maybe (fromJust)
import qualified Data.Tuple (uncurry)
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

getNodeToken :: SyntaxTree.SyntaxTree -> Lexer.Token
getNodeToken = Tree.maybeOnTreeNode (Lexer.Data D.Null) SyntaxTree.token

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

evaluatePrimitiveOperator :: SyntaxTree.SyntaxTree -> D.Data
evaluatePrimitiveOperator tr
  | both D.isNumeric args = case getNodeOperator tr of
    O.Add -> uncurriedNumOperator (+)
    O.Sub -> uncurriedNumOperator (-)
    O.Mult -> uncurriedNumOperator (*)
    O.Div -> uncurriedNumOperator (/)
    O.Eq -> uncurriedBoolOperator (==) numArgVals
    O.Gt -> uncurriedBoolOperator (>) numArgVals
    O.Lt -> uncurriedBoolOperator (<) numArgVals
    O.GtEq -> uncurriedBoolOperator (>=) numArgVals
    O.LtEq -> uncurriedBoolOperator (<=) numArgVals
  | both (D.String "" `LikeClass.like`) args = case getNodeOperator tr of
    O.Add -> uncurriedStringOperator (++)
    O.Eq -> uncurriedBoolOperator (==) stringArgVals
    O.Gt -> uncurriedBoolOperator (>) stringArgVals
    O.Lt -> uncurriedBoolOperator (<) stringArgVals
    O.GtEq -> uncurriedBoolOperator (>=) stringArgVals
    O.LtEq -> uncurriedBoolOperator (<=) stringArgVals
    o ->
      undefinedOperatorBehaviorException
        (O.fromOp o)
        (map show ([fst, snd] <*> [args]))
  | both (D.Boolean True `LikeClass.like`) args = case getNodeOperator tr of
    O.Eq -> uncurriedBoolOperator (==) boolArgVals
    O.Gt -> uncurriedBoolOperator (>) boolArgVals
    O.Lt -> uncurriedBoolOperator (<) boolArgVals
    O.GtEq -> uncurriedBoolOperator (>=) boolArgVals
    O.LtEq -> uncurriedBoolOperator (<=) boolArgVals
    o ->
      undefinedOperatorBehaviorException
        (O.fromOp o)
        (map show ([fst, snd] <*> [args]))
  | otherwise =
    operatorTypeError ((O.fromOp . getNodeOperator) tr) (map show ([fst, snd] <*> [args]))
  where
    args = getOperatorArgs tr
    numArgVals =
      ( (Data.Maybe.fromJust . D.unNum . fst) args,
        (Data.Maybe.fromJust . D.unNum . snd) args
      )
    stringArgVals =
      ( (Data.Maybe.fromJust . D.unString . fst) args,
        (Data.Maybe.fromJust . D.unString . snd) args
      )
    boolArgVals =
      ( (Data.Maybe.fromJust . D.unBoolean . fst) args,
        (Data.Maybe.fromJust . D.unBoolean . snd) args
      )
    getNodeOperator tr' = case getNodeToken tr' of (Lexer.Operator o) -> o; _ -> O.Eq
    uncurriedNumOperator op = D.Num (op `Data.Tuple.uncurry` numArgVals)
    uncurriedStringOperator op = D.String (op `Data.Tuple.uncurry` stringArgVals)
    uncurriedBoolOperator op argVals = D.Boolean (op `Data.Tuple.uncurry` argVals)
    operatorTypeError opString argStrAndType =
      Exception.raiseError $
        Exception.newException
          Exception.OperatorTypeError
          [SyntaxTree.getSyntaxAttributeFromTree SyntaxTree.line tr]
          ( "The operator \'" ++ opString
              ++ "\' cannot be aaplied to the arguments of incompatible types: "
              ++ unwords argStrAndType
              ++ "."
          )
          Exception.Fatal
    undefinedOperatorBehaviorException opString argStrAndType =
      Exception.raiseError $
        Exception.newException
          Exception.UndefinedOperatorBehavior
          [SyntaxTree.getSyntaxAttributeFromTree SyntaxTree.line tr]
          ( "The operator \'" ++ opString
              ++ "\' does not have defined usage for the types of: "
              ++ unwords argStrAndType
              ++ "."
          )
          Exception.Fatal

both :: (a -> Bool) -> (a, a) -> Bool
both f (x, y) = f x && f y

getOperatorArgs :: SyntaxTree.SyntaxTree -> (D.Data, D.Data)
getOperatorArgs tr =
  ( (getValue . head . Tree.treeChildren) tr,
    (getValue . head . tail . Tree.treeChildren) tr
  )
  where
    getValue tr =
      if nodeIsPrimitiveValue tr
        then evaluatePrimitiveData tr
        else evaluatePrimitiveNode tr