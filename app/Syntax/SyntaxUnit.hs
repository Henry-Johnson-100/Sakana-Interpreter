module Syntax.SyntaxUnit
  ( SyntaxUnit (..),
    tokenUnitToSyntaxUnit,
  )
where

import Lexer
import Token.Bracket
import Token.Util.NestedCollapsible

data SyntaxUnit = SyntaxUnit
  { syntaxToken :: Token,
    syntaxLine :: Int,
    syntaxScope :: ScopeType
  }
  deriving (Show, Eq)

tokenUnitToSyntaxUnit :: TokenUnit -> ScopeType -> SyntaxUnit
tokenUnitToSyntaxUnit tu st = SyntaxUnit (unit tu) (unitLine tu) st

-- emptySyntaxGroup :: SyntaxUnit
-- emptySyntaxGroup = SyntaxUnit [] Return

-- bracketNC :: NCCase TokenUnit
-- bracketNC = NCCase (\x -> unit x `elem` [Bracket Send Open, Bracket Return Open]) (\x -> unit x `elem` [Bracket Send Close, Bracket Return Close])

-- tokenUnitsToSyntaxGroups :: [TokenUnit] -> [SyntaxUnit]
-- tokenUnitsToSyntaxGroups [] = []
-- tokenUnitsToSyntaxGroups tus = noSGIfEmpty (partFst part) ++ noSGIfEmpty (partSnd part) ++ tokenUnitsToSyntaxGroups (partThd part)
--   where
--     part = breakByNest bracketNC tus
--     noSGIfEmpty [] = []
--     noSGIfEmpty tus' = [SyntaxUnit tus' (getScopeType tus')]
--     getScopeType tus' = if unit (head tus') `like` genericBracket then getTokenBracketScopeType (unit (head tus')) else Return

-- isBracketNCSyntaxGroup :: SyntaxUnit -> Bool
-- isBracketNCSyntaxGroup sg = isCompleteNestedCollapsible bracketNC $ groupBody sg

-- takeWhileBracketNC :: [SyntaxUnit] -> [SyntaxUnit]
-- takeWhileBracketNC [] = []
-- takeWhileBracketNC sgs = takeWhile (not . isBracketNCSyntaxGroup) sgs

-- dropWhileBracketNC :: [SyntaxUnit] -> [SyntaxUnit]
-- dropWhileBracketNC = dropWhile (not . isBracketNCSyntaxGroup)

-- clusterOnRule :: [SyntaxUnit] -> SyntaxClusterRule -> SyntaxCluster
-- clusterOnRule [] _ = []

-- clusterOnRule
