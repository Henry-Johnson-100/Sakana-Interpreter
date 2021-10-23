module TreeInterpreter.LocalCheck.NodeIs
  ( dataToken,
    nullNode,
    dataTokenAndPrimitive,
    operator,
    fin,
  )
where

import Data.Maybe as DMaybe
import Lexer
import SyntaxTree
import Token.Control as C
import Token.Data as D
import Util.General
import Util.Like as LikeClass

dataToken :: SyntaxUnit -> Bool
dataToken = LikeClass.like Lexer.genericData . SyntaxTree.token

nullNode :: SyntaxUnit -> Bool
nullNode (SyntaxTree.SyntaxUnit (Lexer.Data (D.Null)) _ _) = True
nullNode _ = False

dataTokenAndPrimitive :: SyntaxUnit -> Bool
dataTokenAndPrimitive =
  foldIdApplicativeOnSingleton
    all
    [ dataToken,
      (DMaybe.maybe False D.isPrimitive) . Lexer.baseData . SyntaxTree.token
    ]

operator :: SyntaxUnit -> Bool
operator = LikeClass.like Lexer.genericOperator . SyntaxTree.token

fin :: SyntaxUnit -> Bool
fin = LikeClass.like (Lexer.Control C.Fin) . SyntaxTree.token