module TreeInterpreter.LocalCheck.NodeIs
  ( dataToken,
    nullNode,
    dataTokenAndPrimitive,
    operator,
    fin,
    idNode,
    declarationRequiringId,
    sendContext,
    returnContext,
  )
where

import qualified Data.Maybe as DMaybe
import qualified SakanaParser
import qualified Token.Bracket as B
import qualified Token.Control as C
import qualified Token.Data as D
import qualified Util.General
import qualified Util.Like as LikeClass

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

sendContext :: SakanaParser.SyntaxUnit -> Bool
sendContext = (==) B.Send . SakanaParser.context

returnContext :: SakanaParser.SyntaxUnit -> Bool
returnContext = (==) B.Return . SakanaParser.context