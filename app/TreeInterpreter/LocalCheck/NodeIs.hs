module TreeInterpreter.LocalCheck.NodeIs
  ( dataToken,
    nullNode,
    dataTokenAndPrimitive,
    operator,
    fin,
    idNode,
    declarationRequiringId,
  )
where

import qualified Data.Maybe as DMaybe (maybe)
import qualified Lexer
  ( Token (Control, Data),
    baseData,
    dataTokenIsId,
    genericData,
    genericOperator,
    keywordTokenIsDeclarationRequiringId,
  )
import qualified SyntaxTree (SyntaxUnit (SyntaxUnit, token))
import qualified Token.Control as C (Control (Fin))
import qualified Token.Data as D (Data (Null), isPrimitive)
import qualified Util.General (foldIdApplicativeOnSingleton)
import qualified Util.Like as LikeClass (Like (like))

dataToken :: SyntaxTree.SyntaxUnit -> Bool
dataToken = LikeClass.like Lexer.genericData . SyntaxTree.token

nullNode :: SyntaxTree.SyntaxUnit -> Bool
nullNode (SyntaxTree.SyntaxUnit (Lexer.Data (D.Null)) _ _) = True
nullNode _ = False

dataTokenAndPrimitive :: SyntaxTree.SyntaxUnit -> Bool
dataTokenAndPrimitive =
  Util.General.foldIdApplicativeOnSingleton
    all
    [ dataToken,
      (DMaybe.maybe False D.isPrimitive) . Lexer.baseData . SyntaxTree.token
    ]

operator :: SyntaxTree.SyntaxUnit -> Bool
operator = LikeClass.like Lexer.genericOperator . SyntaxTree.token

fin :: SyntaxTree.SyntaxUnit -> Bool
fin = LikeClass.like (Lexer.Control C.Fin) . SyntaxTree.token

idNode :: SyntaxTree.SyntaxUnit -> Bool
idNode = Lexer.dataTokenIsId . SyntaxTree.token

declarationRequiringId :: SyntaxTree.SyntaxUnit -> Bool
declarationRequiringId =
  Lexer.keywordTokenIsDeclarationRequiringId . SyntaxTree.token