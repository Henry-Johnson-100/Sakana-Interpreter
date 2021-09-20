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
tokenUnitIsFollowedBySendBrackets [] = False
tokenUnitIsFollowedBySendBrackets tus 
    | null takenNestFirstComplete = False
    | otherwise = (==) (Bracket Send Open) $ unit $ head $ takenNestFirstComplete
    where
        takenNestFirstComplete = takeNestFirstComplete bracketNC tus


takeThroughArbitrarySends :: [TokenUnit] -> [TokenUnit]
takeThroughArbitrarySends [] = []
takeThroughArbitrarySends tus
    | tokenUnitIsFollowedBySendBrackets (partThd part) = partFstSnd ++ (takeThroughArbitrarySends (partThd part))
    | otherwise                             = partFstSnd
    where
        part = breakByNest bracketNC tus
        partFstSnd = (partFst part) ++ (partSnd part)


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


mapGenerateParseTreeToParallelGroups :: [[TokenUnit]] -> [ParseTree TokenUnit]
mapGenerateParseTreeToParallelGroups [[]] = []
mapGenerateParseTreeToParallelGroups ttuuss = map (\tus -> generateParseTree tus (tree (head tus))) (filter (\tus -> not (null tus)) ttuuss)


mapGenerateParseTreeToParallelGroupsSuppliedParent :: [[TokenUnit]] -> ParseTree TokenUnit -> [ParseTree TokenUnit]
mapGenerateParseTreeToParallelGroupsSuppliedParent [[]] _ = []
mapGenerateParseTreeToParallelGroupsSuppliedParent ttuuss parentTree = map (\tus -> generateParseTree tus parentTree) (filter (\tus -> not (null tus)) ttuuss)


generateParseTree :: [TokenUnit] -> ParseTree TokenUnit -> ParseTree TokenUnit
generateParseTree []  parentTree = parentTree
generateParseTree (tu:tus) parentTree
    | null tus                         = parentTree -<- (tree tu)
    | beginsWithKeywordExpectingReturn = generateParseTree (dropInfix takenThroughReturn (tu:tus)) (parentTree -<- generateParseTreeKeywordDefinition (tu:tus))
    | beginsWithFinControl             = generateParseTree (dropInfix (partFstSnd) (tu:tus)) (parentTree -<- generateParseTreeControlFin (tu:tus))
    | beginsWithFunctionCall           = generateParseTree (dropInfix takenThroughArbitrarySend (tu:tus)) (parentTree -<- generateParseTreeFunctionCall (tu:tus))
    | beginsWithBracketNest            = generateParseTree (dropInfix (takenBracketNest) (tu:tus)) (parentTree -<- generateParseTreeBracketGroup (takenBracketNest))
    | otherwise                        = generateParseTree tus (parentTree -<- (tree tu))
    -- | otherwise                        = parentTree -<- generateParseTree tus (tree tu)
    where
        beginsWithKeywordExpectingReturn = (unit tu) `like` genericKeyword
        beginsWithFinControl             = (unit tu) == (Control Fin)
        beginsWithFunctionCall           = (unit tu) `like` genericOperator || (dataTokenIsId (unit tu))
        beginsWithBracketNest            = nestedCollapsibleIsPrefixOf bracketNC (tu:tus)
        takenThroughReturn               = takeTokenUnitsThroughReturn (tu:tus)
        takenThroughArbitrarySend        = takeThroughArbitrarySends (tu:tus)
        takenBracketNest                 = takeNestFirstComplete bracketNC (tu:tus)
        part                             = breakByNest bracketNC (tu:tus)
        partFstSnd                       = (partFst part) ++ (partSnd part)


generateParseTreeKeywordDefinition :: [TokenUnit] -> ParseTree TokenUnit
generateParseTreeKeywordDefinition [] = Empty
generateParseTreeKeywordDefinition (tu:tus) = (tree tu) -<- (tree (head tus)) -<= mapGenerateParseTreeToParallelGroups (keywordBracketGroups) where
    keywordBracketGroups = groupCurrentTopLevel $ dropWhileList (\xs -> not (nestedCollapsibleIsPrefixOf bracketNC xs)) $ takeTokenUnitsThroughReturn tus


generateParseTreeFunctionCall :: [TokenUnit] -> ParseTree TokenUnit
generateParseTreeFunctionCall [] = Empty
generateParseTreeFunctionCall (tu:tus) = (tree tu) -<= mapGenerateParseTreeToParallelGroups functionCallBracketGroups where
    functionCallBracketGroups = filter (\x -> not (null x)) $ groupCurrentTopLevel $ dropWhileList (\xs -> not (nestedCollapsibleIsPrefixOf bracketNC xs)) $ takeThroughArbitrarySends tus


generateParseTreeBracketGroup :: [TokenUnit] -> ParseTree TokenUnit
generateParseTreeBracketGroup [] = Empty
generateParseTreeBracketGroup (tu:tus) = if any (\x -> (unit x) == (Data (Punct ","))) (tu:tus) 
                                              then
                                                  (tree tu) -<= mapGenerateParseTreeToParallelGroups ((splitOn (\x -> (unit x) == (Data (Punct ","))) (getNestedCollapsibleContents bracketNC (tu:tus))))
                                              else
                                                  generateParseTree (getNestedCollapsibleContents bracketNC (tu:tus)) (tree tu)


generateParseTreeControlFin :: [TokenUnit] -> ParseTree TokenUnit
generateParseTreeControlFin [] = Empty
generateParseTreeControlFin (tu:tus) = (tree tu) -<- generateParseTreeBracketGroup (takeNestFirstComplete bracketNC (tu:tus))