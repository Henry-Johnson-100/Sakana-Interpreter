module ExecutionTree
  (calc'
  )
where

import qualified Data.Char (isSpace)
import qualified Data.Maybe (fromJust, fromMaybe, isNothing, maybe)
import qualified Data.Tuple (uncurry)
import qualified Exception.Base as Exception
import qualified Lexer
import qualified SyntaxTree
import qualified SyntaxTree as SyntaxUnit (SyntaxUnit (context, line, token))
import qualified Token.Bracket as B
import qualified Token.Control as C
import qualified Token.Data as D
import qualified Token.Keyword as K
import qualified Token.Operator as O
import qualified Token.Util.Like as LikeClass
import qualified Token.Util.Tree as Tree

-- | Unsure if this should be an instance but I will keep it for now
class Truthy a where
  truthy :: a -> Bool
  falsy :: a -> Bool

instance Truthy D.Data where
  truthy (D.Num x) = x == 1.0
  truthy (D.String x) = (not . all Data.Char.isSpace) x && (not . null) x
  truthy (D.Boolean x) = x
  truthy _ = False
  falsy = not . truthy

s' =
  "fish add >(n)> >(m)> <(+ >(n)> >(m)> )<"
    ++ " fish sub >(n)> >(m)> <(- >(n)> >(m)> )< <(add >(1)> >(1)>)<"

t' = Lexer.tokenize s'

pt' = SyntaxTree.generateSyntaxTree t'

calct' =
  head
    . Tree.treeChildren
    . SyntaxTree.generateSyntaxTree
    . Lexer.tokenize

calc' :: String -> D.Data
calc' =
  evaluatePrimitiveNode
    . calct'

{-The most fundamental node execution is returning a primitive value
After that, performing a primitive operation like addition or subtraction
After that, calling a built-in function,
After that, defining a function,
After that, calling a defined function.-}

-- | The main entry point as of right now
evaluatePrimitiveNode :: SyntaxTree.SyntaxTree -> D.Data
evaluatePrimitiveNode Tree.Empty = D.Null
evaluatePrimitiveNode tr
  | nodeStrictlySatisfies nodeIsDataToken tr
      && (D.isPrimitive . getNodeTokenBaseData) tr =
    evaluatePrimitiveData tr
  | nodeStrictlySatisfies nodeIsOperator tr = evaluatePrimitiveOperator tr
  | nodeStrictlySatisfies nodeIsFin tr = evaluateFin tr
  where
    nodeStrictlySatisfies = Tree.maybeOnTreeNode False

evaluatePrimitiveData :: SyntaxTree.SyntaxTree -> D.Data
evaluatePrimitiveData = getNodeTokenBaseData

evaluatePrimitiveOperator :: SyntaxTree.SyntaxTree -> D.Data
evaluatePrimitiveOperator tr
  | both D.isNumeric args = case getNodeOperator tr of
    O.Add -> uncurryArgsToNumOperator (+)
    O.Sub -> uncurryArgsToNumOperator (-)
    O.Mult -> uncurryArgsToNumOperator (*)
    O.Div -> uncurryArgsToNumOperator (/)
    O.Pow -> uncurryArgsToNumOperator (**)
    O.Eq -> uncurryArgsToBoolOperator (==) numArgVals
    O.Gt -> uncurryArgsToBoolOperator (>) numArgVals
    O.Lt -> uncurryArgsToBoolOperator (<) numArgVals
    O.GtEq -> uncurryArgsToBoolOperator (>=) numArgVals
    O.LtEq -> uncurryArgsToBoolOperator (<=) numArgVals
  | both (D.String "" `LikeClass.like`) args = case getNodeOperator tr of
    O.Add -> uncurryArgsToStringOperator (++)
    O.Eq -> uncurryArgsToBoolOperator (==) stringArgVals
    O.Gt -> uncurryArgsToBoolOperator (>) stringArgVals
    O.Lt -> uncurryArgsToBoolOperator (<) stringArgVals
    O.GtEq -> uncurryArgsToBoolOperator (>=) stringArgVals
    O.LtEq -> uncurryArgsToBoolOperator (<=) stringArgVals
    o ->
      undefinedOperatorBehaviorException
        (O.fromOp o)
        (map show ([fst, snd] <*> [args]))
  | both (D.Boolean True `LikeClass.like`) args = case getNodeOperator tr of
    O.Eq -> uncurryArgsToBoolOperator (==) boolArgVals
    O.Gt -> uncurryArgsToBoolOperator (>) boolArgVals
    O.Lt -> uncurryArgsToBoolOperator (<) boolArgVals
    O.GtEq -> uncurryArgsToBoolOperator (>=) boolArgVals
    O.LtEq -> uncurryArgsToBoolOperator (<=) boolArgVals
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
    uncurryArgsToNumOperator op = D.Num (op `Data.Tuple.uncurry` numArgVals)
    uncurryArgsToStringOperator op = D.String (op `Data.Tuple.uncurry` stringArgVals)
    uncurryArgsToBoolOperator op argVals = D.Boolean (op `Data.Tuple.uncurry` argVals)
    operatorTypeError opString argStrAndType =
      Exception.raiseError $
        Exception.newException
          Exception.OperatorTypeError
          [SyntaxTree.getSyntaxAttributeFromTree SyntaxUnit.line tr]
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
          [SyntaxTree.getSyntaxAttributeFromTree SyntaxUnit.line tr]
          ( "The operator \'" ++ opString
              ++ "\' does not have defined usage for the types of: "
              ++ unwords argStrAndType
              ++ "."
          )
          Exception.Fatal

evaluateFin :: SyntaxTree.SyntaxTree -> D.Data
evaluateFin tr = if (truthy . (!! 0)) args then args !! 1 else args !! 2
  where
    args = getNodeArgs tr

nodeIsDataToken :: SyntaxUnit.SyntaxUnit -> Bool
nodeIsDataToken = LikeClass.like Lexer.genericData . SyntaxUnit.token

nodeIsOperator :: SyntaxUnit.SyntaxUnit -> Bool
nodeIsOperator = LikeClass.like Lexer.genericOperator . SyntaxUnit.token

nodeIsFin :: SyntaxUnit.SyntaxUnit -> Bool
nodeIsFin = LikeClass.like (Lexer.Control C.Fin) . SyntaxUnit.token

getNodeTokenBaseData :: SyntaxTree.SyntaxTree -> D.Data
getNodeTokenBaseData =
  Tree.maybeOnTreeNode
    D.Null
    (Data.Maybe.fromMaybe D.Null . (Lexer.baseData . SyntaxUnit.token))

getNodeToken :: SyntaxTree.SyntaxTree -> Lexer.Token
getNodeToken = Tree.maybeOnTreeNode (Lexer.Data D.Null) SyntaxUnit.token

nodeIsPrimitiveValue :: SyntaxTree.SyntaxTree -> Bool
nodeIsPrimitiveValue tr =
  Tree.maybeOnTreeNode False nodeIsDataToken tr
    && (D.isPrimitive . getNodeTokenBaseData) tr

both :: (a -> Bool) -> (a, a) -> Bool
both f (x, y) = f x && f y

getNodeArgs :: SyntaxTree.SyntaxTree -> [D.Data]
getNodeArgs = map evaluatePrimitiveNode . Tree.treeChildren

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