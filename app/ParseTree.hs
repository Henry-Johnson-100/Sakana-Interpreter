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
tokenUnitIsFollowedBySendBrackets tus = (==) (Bracket Send Open) $ unit $ head $ takeNestFirstComplete bracketNC tus


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


-- |Assuming that all the grammar is perfect and logical, probably not a great assumption to make in the infancy of a language -_-
-- groupTopLevelGrammatically :: [TokenUnit] -> [[TokenUnit]]
-- groupTopLevelGrammatically [] = []
-- groupTopLevelGrammatically (tu:tus)
--     | beginsWithKeywordExpectingReturn = (takenThroughReturn)        : (groupTopLevelGrammatically (dropInfix (takenThroughReturn)        (tu:tus)))
--     | beginsWithFinControl             = (partFstSnd)                : (groupTopLevelGrammatically (partThd (part)))
--     | beginsWithFunctionCall           = (takenThroughArbitrarySend) : (groupTopLevelGrammatically (dropInfix (takenThroughArbitrarySend) (tu:tus)))
--     | beginsWithBracketNest            = (takenBracketNest)          : (groupTopLevelGrammatically (dropInfix (takenBracketNest)          (tu:tus)))
--     | otherwise                        = [tu]                        : (groupTopLevelGrammatically tus)
--     where
--         beginsWithKeywordExpectingReturn = (unit tu) `like` genericKeyword
--         beginsWithFinControl             = (unit tu) == (Control Fin)
--         beginsWithFunctionCall           = (unit tu) `like` genericOperator || ((dataTokenIsId (unit tu)) && (tokenUnitIsFollowedBySendBrackets (tu:tus)))
--         beginsWithBracketNest            = nestedCollapsibleIsPrefixOf bracketNC (tu:tus)
--         takenThroughReturn               = takeTokenUnitsThroughReturn (tu:tus)
--         takenThroughArbitrarySend        = takeThroughArbitrarySends (tu:tus)
--         takenBracketNest                 = takeNestFirstComplete bracketNC (tu:tus)
--         part                             = breakByNest bracketNC (tu:tus)
--         partFstSnd                       = (partFst part) ++ (partSnd part)


mapGenerateParseTreeToParallelGroups :: [[TokenUnit]] -> [ParseTree TokenUnit]
mapGenerateParseTreeToParallelGroups [[]] = []
mapGenerateParseTreeToParallelGroups ttuuss = map (\tus -> generateParseTree tus (tree (head tus))) (filter (\x -> not (null x)) ttuuss)


generateParseTree :: [TokenUnit] -> ParseTree TokenUnit -> ParseTree TokenUnit
generateParseTree []  parentTree = parentTree
generateParseTree (tu:tus) parentTree
    | beginsWithKeywordExpectingReturn = parentTree -<- generateParseTree tus (tree tu)
    | beginsWithFinControl             = parentTree -<- getFinAndFollowingBracketGroupTree
    | beginsWithFunctionCall           = parentTree -<- getFunctionCallIdTree -<= getFunctionCallArbitrarySendTrees
    | beginsWithBracketNest            = if bracketNestHasArgs
                                            then
                                                parentTree -<= getParallelArgumentTrees
                                            else
                                                parentTree -<- getSingleArgBracketNestTree
    | otherwise                        = generateParseTree tus (parentTree -<- (tree tu))
    where
        beginsWithKeywordExpectingReturn = (unit tu) `like` genericKeyword
        beginsWithFinControl             = (unit tu) == (Control Fin)
        beginsWithFunctionCall           = (unit tu) `like` genericOperator || ((dataTokenIsId (unit tu)) && (tokenUnitIsFollowedBySendBrackets (tu:tus)))
        beginsWithBracketNest            = nestedCollapsibleIsPrefixOf bracketNC (tu:tus)
        bracketNestHasArgs               = any (tuIsComma) takenBracketNest
        takenThroughReturn               = takeTokenUnitsThroughReturn (tu:tus)
        takenThroughArbitrarySend        = takeThroughArbitrarySends (tu:tus)
        takenBracketNest                 = takeNestFirstComplete bracketNC (tu:tus)
        part                             = breakByNest bracketNC (tu:tus)
        partFstSnd                       = (partFst part) ++ (partSnd part)
        keywordTree                      = (tree tu)
        getFunctionCallIdTree            = (tree tu)
        keywordIdTree                    = (tree (head tus))
        getKeywordBracketGroupTrees      = mapGenerateParseTreeToParallelGroups $ groupCurrentTopLevel takenThroughReturn
        getFunctionCallArbitrarySendTrees = mapGenerateParseTreeToParallelGroups $ groupCurrentTopLevel takenThroughArbitrarySend
        getFinAndFollowingBracketGroupTree = generateParseTree (partSnd part) (tree tu)
        getParallelArgumentTrees         = mapGenerateParseTreeToParallelGroups (splitOn (tuIsComma) (getNestedCollapsibleContents bracketNC takenBracketNest))
        getSingleArgBracketNestTree      = generateParseTree (getNestedCollapsibleContents bracketNC takenBracketNest) (tree tu)
        tuIsComma :: TokenUnit -> Bool
        tuIsComma x                      = (unit x) == (Data (Punct ","))