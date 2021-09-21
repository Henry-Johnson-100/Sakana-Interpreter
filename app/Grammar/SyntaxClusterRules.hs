module Grammar.SyntaxClusterRules (
    SyntaxClusterRule(..),
    syntaxClusterRule
) where

import Lexer
import Token.Bracket
import Token.Keyword
import Token.Control
import Token.Operator

data SyntaxBracketGroupRule = SBGR {
    sbgrScopeType :: ScopeType,
    argSets       :: Int
} deriving (Show, Eq)

argsRequired :: SyntaxBracketGroupRule -> Bool
argsRequired sbgr = argSets sbgr > -1

generalSBGR :: ScopeType -> SyntaxBracketGroupRule
generalSBGR Send = SBGR Send (-1)
generalSBGR _    = SBGR Return 1

data SyntaxClusterRule = SCR {
    clusterType :: Token,
    sendRules :: [SyntaxBracketGroupRule],
    returnRules :: [SyntaxBracketGroupRule],
    allowArbitrarySends :: Bool
} deriving (Show, Eq)

returnIsRequired :: SyntaxClusterRule -> Bool
returnIsRequired scr = not (null (returnRules scr))

syntaxClusterRule :: Token -> SyntaxClusterRule
syntaxClusterRule t
    | t `like` genericKeyword  = keywordClusterRule t
    | t `like` genericControl  = controlClusterRule t
    | t `like` genericOperator = operatorClusterRule t
    | dataTokenIsId t          = idClusterRule t
    | otherwise                = SCR t [] [] False

keywordClusterRule :: Token -> SyntaxClusterRule
keywordClusterRule (Keyword Fish)    = SCR (Keyword Fish)    [generalSBGR Send]                   [generalSBGR Return] True
keywordClusterRule (Keyword Route)   = SCR (Keyword Route)   [generalSBGR Send]                   [generalSBGR Return] True
keywordClusterRule (Keyword Migrate) = SCR (Keyword Migrate) [SBGR Send 1]                        []                   False
keywordClusterRule (Keyword School)  = SCR (Keyword School)  [generalSBGR Send, generalSBGR Send] [generalSBGR Return] True
keywordClusterRule (Keyword Shoal)   = SCR (Keyword Shoal)   []                                   [generalSBGR Return] True

controlClusterRule :: Token -> SyntaxClusterRule
controlClusterRule (Control Fin) = SCR (Control Fin) [SBGR Send 1] [] False

operatorClusterRule :: Token -> SyntaxClusterRule
operatorClusterRule t = SCR t [SBGR Send 2] [] False

idClusterRule :: Token -> SyntaxClusterRule
idClusterRule t = SCR t [SBGR Send (-1)] [] True