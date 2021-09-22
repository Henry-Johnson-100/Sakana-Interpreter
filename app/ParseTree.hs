module ParseTree
  (
  )
where

import Data.List
import Lexer
import Syntax.SyntaxUnit
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

type ParseTree = BinaryRelationalPolyTree TokenUnit

instance TreeIO BinaryRelationalPolyTree where
  fPrintTree d (BRPT n a b) = concat (replicate (d * 4 - 1) "-") ++ ">" ++ show n ++ "\n" ++ concatMap (fPrintTree (d + 1)) (a ++ b)
  ioPrintTree t = putStrLn $ fPrintTree 0 t

instance Functor BinaryRelationalPolyTree where
  fmap f Empty = Empty
  fmap f (BRPT n a b) = BRPT (f n) (fmap (fmap f) a) (fmap (fmap f) b)

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

listToNestedTree :: [a] -> ScopeType -> BinaryRelationalPolyTree a
listToNestedTree [] _ = Empty
listToNestedTree [x] _ = tree x
listToNestedTree (x : xs) Send = tree x -<.- listToNestedTree xs Send
listToNestedTree (x : xs) Return = tree x -<*- listToNestedTree xs Return

-- -- | append a brpt sg tree to the appropriate child list
-- (-<|-) :: ParseTree -> ParseTree -> ParseTree
-- brptParent -<|- brptToAppend = if syntaxScope (treeBody brptToAppend) == Send then brptParent -<.- brptToAppend else brptParent -<*- brptToAppend

-- (-<|=) :: ParseTree -> [ParseTree] ->ParseTree
-- brptParent -<|= []           = brptParent
-- brptParent -<|= [brpt]       = brptParent -<|- brpt
-- brptParent -<|= (brpt:brpts) = (brptParent -<|- brpt) -<|= brpts

bracketSUNC :: NCCase SyntaxUnit
bracketSUNC = NCCase (\x -> syntaxToken x `elem` [Bracket Send Open, Bracket Return Open]) (\x -> syntaxToken x `elem` [Bracket Send Close, Bracket Return Close])

bracketNC :: NCCase TokenUnit
bracketNC = NCCase (\x -> unit x `elem` [Bracket Send Open, Bracket Return Open]) (\x -> unit x `elem` [Bracket Send Close, Bracket Return Close])

-- mapTokenUnitsToSyntaxUnits :: [TokenUnit] -> [SyntaxUnit]
-- mapTokenUnitsToSyntaxUnits tus = mapTokenUnitsToSyntaxUnits' tus Return where
--   mapTokenUnitsToSyntaxUnits' :: [TokenUnit] -> ScopeType -> [SyntaxUnit]
--   mapTokenUnitsToSyntaxUnits' [] _ = []
--   mapTokenUnitsToSyntaxUnits' (tu:tus) st
--       | isOpeningBracket (unit tu) = tokenUnitToSyntaxUnit tu (getTokenBracketScopeType (unit tu)) : mapTokenUnitsToSyntaxUnits' tus (getTokenBracketScopeType (unit tu))
--       | otherwise                  = tokenUnitToSyntaxUnit tu st : mapTokenUnitsToSyntaxUnits' tus st
--       where
--         isOpeningBracket :: Token -> Bool
--         isOpeningBracket (Bracket _ Open) = True
--         isOpeningBracket _                = False

tokenUnitIsComma :: TokenUnit -> Bool
tokenUnitIsComma (PacketUnit (Data (Punct ",")) _) = True
tokenUnitIsComma _                                 = False

isSubordinator :: TokenUnit -> Bool
isSubordinator tu = unit tu `like` genericKeyword || unit tu `like` genericOperator || dataTokenIsId (unit tu)

makeHeadlessTree :: [TokenUnit] -> ScopeType -> ParseTree
makeHeadlessTree [] _ = Empty
makeHeadlessTree [tu] _ = tree tu
makeHeadlessTree (tu : tus) st
  | isCompleteNestedCollapsible bracketNC (tu : tus) = makeHeadlessTree (getNestedCollapsibleContents bracketNC (tu : tus)) (getTokenBracketScopeType (unit tu))
  | otherwise = insertIntoParseTree tus (tree tu) st

getCompleteBracketNCArguments :: [TokenUnit] -> [[TokenUnit]]
getCompleteBracketNCArguments [] = [[]]
getCompleteBracketNCArguments tus = if any (\x -> Data (Punct ",") == unit x) tus then splitOn (\x -> Data (Punct ",") == unit x) tus else [tus]

generateParseTree :: [TokenUnit] -> ParseTree
generateParseTree tus = insertIntoParseTree tus (tree (PacketUnit (Data (Id "main")) 0)) Return

insertIntoParseTree :: [TokenUnit] -> ParseTree -> ScopeType -> ParseTree
insertIntoParseTree [] parent _ = parent
insertIntoParseTree (tu' : tus') parent st
  | isSubordinator tu' = parent -<|- insertIntoParseTree tus' (tree tu') st
  | nestedCollapsibleIsPrefixOf bracketNC (tu' : tus') = insertIntoParseTree (dropInfix prefixedNC (tu':tus')) (parent -<|= map (`makeHeadlessTree` st) (getCompleteBracketNCArguments (getNestedCollapsibleContents bracketNC prefixedNC))) (getTokenBracketScopeType (unit tu')) --This line is the issue right now
  | otherwise = insertIntoParseTree tus' (parent -<|- tree tu') st
  where
    (-<|-) :: ParseTree -> ParseTree -> ParseTree
    parent' -<|- child' = if st == Send then parent' -<.- child' else parent' -<*- child'
    (-<|=) :: ParseTree -> [ParseTree] -> ParseTree
    parent' -<|= [] = parent'
    parent' -<|= [child'] = parent' -<|- child'
    parent' -<|= (child' : children') = (parent' -<|- child') -<|= children'
    bracketPartition = breakByNest bracketNC (tu' : tus')
    prefixedNC = takeNestFirstComplete bracketNC (tu':tus')