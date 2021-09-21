module Syntax.SyntaxGroup (
SyntaxGroup(..)
) where

import Lexer
import Token.Bracket
import Token.Util.NestedCollapsible

data SyntaxGroup = SyntaxGroup
  { groupBody :: [TokenUnit],
    bodyType :: ScopeType
  }
  deriving (Show, Eq)

emptySyntaxGroup :: SyntaxGroup
emptySyntaxGroup = SyntaxGroup [] Return

bracketNC :: NCCase TokenUnit
bracketNC = NCCase (\x -> unit x `elem` [Bracket Send Open, Bracket Return Open]) (\x -> unit x `elem` [Bracket Send Close, Bracket Return Close])

tokenUnitsToSyntaxGroups :: [TokenUnit] -> [SyntaxGroup]
tokenUnitsToSyntaxGroups [] = []
tokenUnitsToSyntaxGroups tus = noSGIfEmpty (partFst part) ++ noSGIfEmpty (partSnd part) ++ tokenUnitsToSyntaxGroups (partThd part)
  where
    part = breakByNest bracketNC tus
    noSGIfEmpty [] = []
    noSGIfEmpty tus' = [SyntaxGroup tus' (getScopeType tus')]
    getScopeType tus' = if unit (head tus') `like` genericBracket then getTokenBracketScopeType (unit (head tus')) else Return

isBracketNCSyntaxGroup :: SyntaxGroup -> Bool
isBracketNCSyntaxGroup sg = isCompleteNestedCollapsible bracketNC $ groupBody sg

takeWhileBracketNC :: [SyntaxGroup] -> [SyntaxGroup]
takeWhileBracketNC [] = []
takeWhileBracketNC sgs = takeWhile (not . isBracketNCSyntaxGroup) sgs

dropWhileBracketNC :: [SyntaxGroup] -> [SyntaxGroup]
dropWhileBracketNC = dropWhile (not . isBracketNCSyntaxGroup)

-- | clusters syntax groups based on defined grammar rules
-- takeSyntaxCluster :: [SyntaxGroup] -> [SyntaxGroup]
-- takeSyntaxCluster [] = []
-- takeSyntaxCluster sgs
--     | not (isBracketNCSyntaxGroup (head sgs)) = head sgs : takeSyntaxCluster (tail sgs)
--     | otherwise                         = [head sgs]