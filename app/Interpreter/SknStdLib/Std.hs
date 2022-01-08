{-# LANGUAGE MagicHash #-}

module Interpreter.SknStdLib.Std
  ( module Interpreter.SknStdLib.Type,
    exporting,
  )
where

import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Exception.Base as Exception
import qualified Interpreter.Inspection as Inspect
import qualified Interpreter.SknStdLib.IO
import Interpreter.SknStdLib.Type
import qualified Parser.Core
import qualified Parser.Syntax as Syntax
import qualified Util.Classes as UC
import qualified Util.General as UGen
import Util.Tree (Tree ((:-<-:)))
import qualified Util.Tree as Tree

exporting :: [SknStdLibFunction]
exporting =
  stdFunctions
    ++ Interpreter.SknStdLib.IO.exporting

stdFunctions :: [SknStdLibFunction]
stdFunctions =
  concat
    [ algebraicFunctions,
      ordFunctions,
      generalFunctions
    ]

----StdLib function declarations----------------------------------------------------------
------------------------------------------------------------------------------------------

algebraicFunctions :: [SknStdLibFunction]
algebraicFunctions =
  [ skn_sum,
    skn_product,
    skn_div,
    skn_subtract,
    skn_pow
  ]

skn_sum :: SknStdLibFunction
skn_sum = genericFoldingAlgebraicStdLibFunc "+" (+)

skn_product :: SknStdLibFunction
skn_product = genericFoldingAlgebraicStdLibFunc "*" (*)

skn_div :: SknStdLibFunction
skn_div = genericFoldingAlgebraicStdLibFunc "/" (/)

skn_subtract :: SknStdLibFunction
skn_subtract = genericFoldingAlgebraicStdLibFunc "-" (-)

skn_pow :: SknStdLibFunction
skn_pow = genericFoldingAlgebraicStdLibFunc "^" (**)

ordFunctions :: [SknStdLibFunction]
ordFunctions =
  [ skn_eq,
    skn_gt,
    skn_lt,
    skn_gteq,
    skn_lteq
  ]

skn_eq :: SknStdLibFunction
skn_eq = genericOrdCompareStdLibFunc "==" (==) (==) (==)

skn_gt :: SknStdLibFunction
skn_gt = genericOrdCompareStdLibFunc ">" (>) (>) (>)

skn_lt :: SknStdLibFunction
skn_lt = genericOrdCompareStdLibFunc "<" (<) (<) (<)

skn_gteq :: SknStdLibFunction
skn_gteq = genericOrdCompareStdLibFunc ">=" (>=) (>=) (>=)

skn_lteq :: SknStdLibFunction
skn_lteq = genericOrdCompareStdLibFunc "<=" (<=) (<=) (<=)

generalFunctions :: [SknStdLibFunction]
generalFunctions = [skn_show, skn_read]

-- | Take a primitive value and return a string.
skn_show :: SknStdLibFunction
skn_show = GeneralStdLibFunction id# params# skn_show#
  where
    id# = "show_prim"
    params# = ["x"]
    skn_show# trs = skn_show_definition# trs
      where
        skn_show_definition# :: [Syntax.SyntaxTree] -> IO Syntax.SyntaxTree
        skn_show_definition# (tr : []) =
          if Inspect.treeHeadIsPrimitiveData tr
            then
              ( return
                  . Tree.tree
                  . ( \((Syntax.SyntaxUnit (Syntax.Data d) _ _) :-<-: _) ->
                        UC.defaultValue
                          { Syntax.token =
                              (Syntax.Data (Syntax.String (UC.format d)))
                          }
                    )
              )
                tr
            else raiseNonPrimitiveTypeError id# [tr]
        -- raise error for wrong number of arguments
        skn_show_definition# trs = raiseSknStdLibArgumentException trs "" params#

-- | Take a primitive string and return an appropriately typed value.
skn_read :: SknStdLibFunction
skn_read = GeneralStdLibFunction id# params# skn_read#
  where
    id# = "read_prim"
    params# = ["x"]
    skn_read# trs = skn_read_definition# trs
      where
        skn_read_definition# :: [Syntax.SyntaxTree] -> IO Syntax.SyntaxTree
        skn_read_definition# (tr : []) =
          case Tree.treeNode tr of
            Maybe.Just (Syntax.SyntaxUnit (Syntax.Data (Syntax.String str)) l st) ->
              (return . Tree.tree)
                UC.defaultValue
                  { Syntax.token = Syntax.Data (parseSknString str),
                    Syntax.line = l,
                    Syntax.context = st
                  }
            Maybe.Just su -> raiseNonPrimitiveTypeError id# [tr]
            _ -> raiseSknStdLibArgumentException [tr] "" params#
        --raise error for wrong number of arguments
        skn_read_definition# trs = raiseSknStdLibArgumentException trs "" params#
        parseSknString :: String -> Syntax.Data
        parseSknString str =
          Either.either
            ((const . Syntax.String) str)
            id
            ( Parser.Core.generalParsePreserveError
                Parser.Core.dataParser
                "read_prim parsing"
                str
            )

----Functions for generating StdLib functions---------------------------------------------
------------------------------------------------------------------------------------------

-- | Really not a fan of that but I think it's okay for now.
genericOrdCompareStdLibFunc ::
  String ->
  (Double -> Double -> Bool) ->
  (String -> String -> Bool) ->
  (Bool -> Bool -> Bool) ->
  SknStdLibFunction
genericOrdCompareStdLibFunc name doubleCompare strCompare boolCompare =
  GeneralStdLibFunction
    generic_ord_compare_id#
    generic_ord_compare_params#
    generic_ord_compare#
  where
    generic_ord_compare_id# = name
    generic_ord_compare_params# = ["x", "y"]
    generic_ord_compare# trs = generic_ord_compare_definition# trs
      where
        generic_ord_compare_definition# :: [Syntax.SyntaxTree] -> IO Syntax.SyntaxTree
        generic_ord_compare_definition# (trx : try : []) =
          if UGen.both Inspect.treeHeadIsPrimitiveData curryTrees
            then case curryData of
              (Syntax.Num x, Syntax.Num y) -> (return . stBool . doubleCompare x) y
              (Syntax.String x, Syntax.String y) -> (return . stBool . strCompare x) y
              (Syntax.Boolean x, Syntax.Boolean y) -> (return . stBool . boolCompare x) y
              _ -> raiseMismatchedPrimitiveComparitiveTypeError trs
            else raiseNonPrimitiveTypeError name trs
          where
            curryTrees :: (Syntax.SyntaxTree, Syntax.SyntaxTree)
            curryTrees = (trx, try)
            curryData :: (Syntax.Data, Syntax.Data)
            curryData =
              biFunctorApply
                ( Maybe.fromJust
                    . (=<<) (Syntax.baseData . Syntax.token)
                    . Tree.treeNode
                )
                curryTrees
            biFunctorApply :: (a -> b) -> (a, a) -> (b, b)
            biFunctorApply f (x1, x2) = (f x1, f x2)
            stBool :: Bool -> Syntax.SyntaxTree
            stBool b =
              Tree.tree
                (UC.defaultValue {Syntax.token = (Syntax.Data (Syntax.Boolean b))})
        -- Raise exception if number of arguments passed into the function is not two.
        generic_ord_compare_definition# _ =
          raiseSknStdLibArgumentException trs "" generic_ord_compare_params#

genericFoldingAlgebraicStdLibFunc ::
  String -> (Double -> Double -> Double) -> SknStdLibFunction
genericFoldingAlgebraicStdLibFunc name foldingFunction =
  GeneralStdLibFunction name general_params# general_fold#
  where
    general_params# = ["x1", "x2", ".."]
    general_fold# trs = general_fold_definition# trs
      where
        general_fold_definition# :: [Syntax.SyntaxTree] -> IO Syntax.SyntaxTree
        -- raise exception if only one argument is passed into the function.
        general_fold_definition# (tr : []) =
          raiseSknStdLibArgumentException
            [tr]
            ("Expecting two or more arguments to the " ++ name ++ " function")
            (general_params#)
        general_fold_definition# trs
          | any (not . Inspect.treeHeadIsPrimitiveNumType) trs =
            (raiseAlgebraicTypeError . filter (not . Inspect.treeHeadIsPrimitiveNumType))
              trs
          | otherwise = (return . foldDoubleTreesWith foldingFunction) trs

raiseMismatchedPrimitiveComparitiveTypeError :: [Tree Syntax.SyntaxUnit] -> a2
raiseMismatchedPrimitiveComparitiveTypeError trs =
  Exception.raiseError
    ( Exception.newException
        Exception.TypeException
        ((map Syntax.line . concatMap Tree.flattenTree) trs)
        ( "Cannot compare values of two different types: "
            ++ (unlines . map UC.format) trs
        )
        Exception.Fatal
    )

raiseNonPrimitiveTypeError :: [Char] -> [Tree Syntax.SyntaxUnit] -> a2
raiseNonPrimitiveTypeError opName trs =
  Exception.raiseError
    ( Exception.newException
        Exception.TypeException
        ((map Syntax.line . concatMap Tree.flattenTree) trs)
        ( "Cannot use this operation, "
            ++ opName
            ++ " on non-primitive values: "
            ++ (unlines . map UC.format) trs
        )
        Exception.Fatal
    )

raiseAlgebraicTypeError :: [Tree Syntax.SyntaxUnit] -> a2
raiseAlgebraicTypeError trs =
  Exception.raiseError
    ( Exception.newException
        Exception.TypeException
        ((map Syntax.line . concatMap Tree.flattenTree) trs)
        ( "Expecting only arguments of type \'Num\' but got: "
            ++ (unlines . map UC.format) trs
        )
        Exception.Fatal
    )

foldDoubleTreesWith ::
  (Double -> Double -> Double) ->
  [Syntax.SyntaxTree] ->
  Tree Syntax.SyntaxUnit
foldDoubleTreesWith foldBinOp = foldWith foldBinOp . map getDoubleFromTree
  where
    -- Partial function that will only be called when it is confirmed that
    -- all trees are of the type 'Num'
    getDoubleFromTree :: Syntax.SyntaxTree -> Double
    getDoubleFromTree
      ((Syntax.SyntaxUnit (Syntax.Data (Syntax.Num d)) _ _) :-<-: _) = d
    foldWith :: (Double -> Double -> Double) -> [Double] -> Tree Syntax.SyntaxUnit
    foldWith foldBinOp (d : ds) =
      Tree.tree
        ( UC.defaultValue
            { Syntax.token =
                Syntax.Data (Syntax.Num (List.foldl' foldBinOp d ds))
            }
        )
