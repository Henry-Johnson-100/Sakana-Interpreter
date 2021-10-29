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
import qualified SakanaParser
  ( SyntaxUnit (SyntaxUnit, token),
    Token (Control, Data),
    baseData,
    dataTokenIsId,
    genericData,
    genericOperator,
    keywordTokenIsDeclarationRequiringId,
  )
import qualified Token.Control as C (Control (Fin))
import qualified Token.Data as D (Data (Null), isPrimitive)
import qualified Util.General (foldIdApplicativeOnSingleton)
import qualified Util.Like as LikeClass (Like (like))

dataToken :: SakanaParser.SyntaxUnit -> Bool
dataToken = LikeClass.like SakanaParser.genericData . SakanaParser.token

nullNode :: SakanaParser.SyntaxUnit -> Bool
nullNode (SakanaParser.SyntaxUnit (SakanaParser.Data (D.Null)) _ _) = True
nullNode _ = False

dataTokenAndPrimitive :: SakanaParser.SyntaxUnit -> Bool
dataTokenAndPrimitive =
  Util.General.foldIdApplicativeOnSingleton
    all
    [ dataToken,
      (DMaybe.maybe False D.isPrimitive) . SakanaParser.baseData . SakanaParser.token
    ]

operator :: SakanaParser.SyntaxUnit -> Bool
operator = LikeClass.like SakanaParser.genericOperator . SakanaParser.token

fin :: SakanaParser.SyntaxUnit -> Bool
fin = LikeClass.like (SakanaParser.Control C.Fin) . SakanaParser.token

idNode :: SakanaParser.SyntaxUnit -> Bool
idNode = SakanaParser.dataTokenIsId . SakanaParser.token

declarationRequiringId :: SakanaParser.SyntaxUnit -> Bool
declarationRequiringId =
  SakanaParser.keywordTokenIsDeclarationRequiringId . SakanaParser.token