module ParseTree
  (
  )
where

import Data.List
import Lexer
import Token.Bracket
import Token.Control
import Token.Data
import Token.Keyword
import Token.Operator
import Token.Util.EagerCollapsible
import Token.Util.NestedCollapsible

class TreeIO r where
  fPrintTree :: (Show a) => Int -> r a -> String
  ioPrintTree :: (Show a) => r a -> IO ()

data BinaryRelationalPolyTree a
  = Empty
  | BRPT
      { treeBody :: a,
        aChildren :: [BinaryRelationalPolyTree a],
        bChildren :: [BinaryRelationalPolyTree a]
      }
  deriving (Show, Eq)

type SyntaxTree = BinaryRelationalPolyTree SyntaxGroup

instance TreeIO BinaryRelationalPolyTree where
  fPrintTree d (BRPT n a b) = concat (replicate (d * 4 - 1) "-") ++ ">" ++ show n ++ "\n" ++ concatMap (fPrintTree (d + 1)) (a ++ b)
  ioPrintTree t = putStrLn $ fPrintTree 0 t

instance Functor BinaryRelationalPolyTree where
  fmap f Empty = Empty
  fmap f (BRPT n a b) = BRPT (f n) (fmap (fmap f) a) (fmap (fmap f) b)

data SyntaxGroup = SyntaxGroup
  { groupBody :: [TokenUnit],
    bodyType :: ScopeType
  }
  deriving (Show, Eq)

emptySyntaxGroup :: SyntaxGroup
emptySyntaxGroup = SyntaxGroup [] Return

tree :: a -> BinaryRelationalPolyTree a
tree x = BRPT x [] []

-- | Append a BRPT to aChildren list
(-<.-) :: BinaryRelationalPolyTree a -> BinaryRelationalPolyTree a -> BinaryRelationalPolyTree a
(BRPT n a b) -<.- brpt = BRPT n (a ++ [brpt]) b

-- | Extend an aChildren BRPT list
(-<.=) :: BinaryRelationalPolyTree a -> [BinaryRelationalPolyTree a] -> BinaryRelationalPolyTree a
(BRPT n a b) -<.= brpt = BRPT n (a ++ brpt) b

-- | Append a BRPT to bChildren List
(-<*-) :: BinaryRelationalPolyTree a -> BinaryRelationalPolyTree a -> BinaryRelationalPolyTree a
(BRPT n a b) -<*- brpt = BRPT n a (b ++ [brpt])

-- | Extend a bChildren BRPT list
(-<*=) :: BinaryRelationalPolyTree a -> [BinaryRelationalPolyTree a] -> BinaryRelationalPolyTree a
(BRPT n a b) -<*= brpt = BRPT n a (b ++ brpt)

bracketNC :: NCCase TokenUnit
bracketNC = NCCase (\x -> unit x `elem` [Bracket Send Open, Bracket Return Open]) (\x -> unit x `elem` [Bracket Send Close, Bracket Return Close])

listToNestedTree :: [a] -> ScopeType -> BinaryRelationalPolyTree a
listToNestedTree [] _ = Empty
listToNestedTree [x] _ = tree x
listToNestedTree (x : xs) Send = tree x -<.- listToNestedTree xs Send
listToNestedTree (x : xs) Return = tree x -<*- listToNestedTree xs Return

tokenUnitsToSyntaxGroups :: [TokenUnit] -> [SyntaxGroup]
tokenUnitsToSyntaxGroups [] = []
tokenUnitsToSyntaxGroups tus = noSGIfEmpty (partFst part) ++ noSGIfEmpty (partSnd part) ++ tokenUnitsToSyntaxGroups (partThd part)
  where
    part = breakByNest bracketNC tus
    noSGIfEmpty [] = []
    noSGIfEmpty tus' = [SyntaxGroup tus' (getScopeType tus')]
    getScopeType tus' = if unit (head tus') `like` genericBracket then getTokenBracketScopeType (unit (head tus')) else Return

-- | clusters syntax groups based on defined grammar rules
-- takeSyntaxCluster :: [SyntaxGroup] -> [SyntaxGroup]
-- takeSyntaxCluster [] = []
-- takeSyntaxCluster sgs
--     | not (isBracketNCSyntaxGroup (head sgs)) = head sgs : takeSyntaxCluster (tail sgs)
--     | otherwise                         = [head sgs]

-- | append a brpt sg tree to the appropriate child list
(-<|-) :: SyntaxTree -> SyntaxTree -> SyntaxTree
brptParent -<|- brptToAppend = if bodyType (treeBody brptToAppend) == Send then brptParent -<.- brptToAppend else brptParent -<*- brptToAppend

(-<|=) :: SyntaxTree -> [SyntaxTree] ->SyntaxTree
brptParent -<|= []           = brptParent
brptParent -<|= [brpt]       = brptParent -<|- brpt
brptParent -<|= (brpt:brpts) = (brptParent -<|- brpt) -<|= brpts

isBracketNCSyntaxGroup :: SyntaxGroup -> Bool
isBracketNCSyntaxGroup sg = isCompleteNestedCollapsible bracketNC $ groupBody sg

takeWhileBracketNC :: [SyntaxGroup] -> [SyntaxGroup]
takeWhileBracketNC [] = []
takeWhileBracketNC sgs = takeWhile (not . isBracketNCSyntaxGroup) sgs

dropWhileBracketNC :: [SyntaxGroup] -> [SyntaxGroup]
dropWhileBracketNC = dropWhile (not . isBracketNCSyntaxGroup)

makeSyntaxGroupTree :: [SyntaxGroup] -> SyntaxTree -> SyntaxTree
makeSyntaxGroupTree [] brpt = brpt
makeSyntaxGroupTree (sg:sgs) brpt
    | isBracketNCSyntaxGroup sg = makeSyntaxGroupTree sgs (brpt -<|- tree sg)
    | otherwise                 = brpt -<|- makeSyntaxGroupTree sgs (tree sg)

makeSyntaxGroupGroupTree :: [[SyntaxGroup]] -> SyntaxTree -> SyntaxTree
makeSyntaxGroupGroupTree [[]] st = st
makeSyntaxGroupGroupTree ssggss st = st -<|= map (`makeSyntaxGroupTree` tree emptySyntaxGroup) ssggss