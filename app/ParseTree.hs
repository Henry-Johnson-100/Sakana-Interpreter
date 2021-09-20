module ParseTree(
ParseTree(..),
TreeIO(..),
generateParseTree
) where


import Lexer
import Token.Bracket
import Token.Control
import Token.Data
import Token.Keyword
import Token.Operator
import Token.Util.NestedCollapsible
import Token.Util.EagerCollapsible (dropInfix)
import Data.List


data ParseTree a = Empty | ParseTree {
    body     :: a,
    children :: [ParseTree a]
} deriving (Show, Read, Eq)


class TreeIO r where
    fPrintTree  :: (Show a) => Int -> r a -> String
    ioPrintTree :: (Show a) => r a -> IO ()


instance TreeIO ParseTree where
    fPrintTree d (ParseTree b cs) = (concat (replicate ((d * 4) - 1) "-")) ++ ">" ++ show b ++ "\n" ++ (concat (map (\c -> fPrintTree (d + 1) c) cs))
    ioPrintTree t = putStrLn $ fPrintTree 0 t


instance Functor ParseTree where
    fmap f (ParseTree b cs) = ParseTree (f b) (map (\c -> fmap f c) cs)


bracketNC :: NCCase TokenUnit
bracketNC = NCCase (\x -> any ((unit x) ==) [Bracket Send Open, Bracket Return Open]) (\x -> any ((unit x) ==) [Bracket Send Close, Bracket Return Close])
bracketReturnNC :: NCCase TokenUnit
bracketReturnNC = NCCase (\x -> (unit x) == (Bracket Return Open)) (\x -> (unit x) == (Bracket Return Close))


nullTree :: ParseTree a -> Bool
nullTree Empty = True
nullTree _     = False


tree :: a -> ParseTree a
tree x = ParseTree x []


-- | Appends a tree to the base tree's children list
(-<-) :: ParseTree a -> ParseTree a -> ParseTree a
(ParseTree b cs) -<- pt = ParseTree b (cs ++ [pt])


-- | Extends a base tree's children list by the given list of trees
(-<=) :: ParseTree a -> [ParseTree a] -> ParseTree a
(ParseTree b cs) -<= pts = ParseTree b (cs ++ pts)


tokenUnitIsFollowedBySendBrackets :: [TokenUnit] -> Bool
tokenUnitIsFollowedBySendBrackets tus = (==) (Bracket Send Open) $ unit $ head $ takeNestFirstComplete bracketNC tus


tokenUnitHasReturnAfterArbitrarySends :: [TokenUnit] -> Bool
tokenUnitHasReturnAfterArbitrarySends []  = False
tokenUnitHasReturnAfterArbitrarySends tus = if (unit (head (partSnd part))) == (Bracket Return Open) then True else tokenUnitHasReturnAfterArbitrarySends (partThd part) where
    part = breakByNest bracketNC tus


takeTokenUnitsThroughReturn :: [TokenUnit] -> [TokenUnit]
takeTokenUnitsThroughReturn [] = []
takeTokenUnitsThroughReturn tus = if (unit (head (partSnd part))) == (Bracket Return Open) then partFstSnd else partFstSnd ++ (takeTokenUnitsThroughReturn (partThd part)) where
    part = breakByNest bracketNC tus
    partFstSnd = (partFst part) ++ (partSnd part)


groupCurrentTopLevel :: [TokenUnit] -> [[TokenUnit]]
groupCurrentTopLevel [] = []
groupCurrentTopLevel tus = partFstSnd : (groupCurrentTopLevel (partThd part)) where
    part = breakByNest bracketNC tus
    partFstSnd = (partFst part) ++ (partSnd part)


mapGenerateParseTreeToParallelGroups :: [[TokenUnit]] -> ParseTree TokenUnit -> [ParseTree TokenUnit]
mapGenerateParseTreeToParallelGroups [[]] _ = []
mapGenerateParseTreeToParallelGroups ttuuss parallelBaseOwner = map (\tus -> generateParseTree tus parallelBaseOwner) ttuuss


generateParseTree :: [TokenUnit] -> ParseTree TokenUnit -> ParseTree TokenUnit
generateParseTree [] base = base
generateParseTree tus base 
    | any (\x -> (unit (head tus)) `like` x) [genericKeyword, genericControl, genericOperator] = base -<- (generateParseTree (tail tus) (tree (head tus)))
    | nestedCollapsibleIsPrefixOf bracketNC tus                                                = generateParseTree (dropInfix (takenFirstCompleteNest) tus) (getFirstNestBracketTree)
    | otherwise                                                                                = generateParseTree (tail tus) (base -<- (tree (head tus)))
    where
        takenFirstCompleteNest :: [TokenUnit]
        takenFirstCompleteNest = takeNestFirstComplete bracketNC tus
        getBracketNCContents :: [TokenUnit] -> [TokenUnit]
        getBracketNCContents tus' = getNestedCollapsibleContents bracketNC tus'
        getFirstNestBracketTree :: ParseTree TokenUnit
        getFirstNestBracketTree = if any (\x -> (Data (Punct ",")) == (unit x)) (takenFirstCompleteNest)
                         then base -<= (map (\tus' -> generateParseTree tus' (tree (head tus))) (splitOn (\x -> (Data (Punct ",")) == (unit x)) (getBracketNCContents (takenFirstCompleteNest))))
                         else base -<- (generateParseTree (getBracketNCContents (takenFirstCompleteNest)) (tree (head tus)))