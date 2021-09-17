module ParseTree(
ParseTree(..),
-- generateParseTree
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


data StratifiedParseTree a = StratEmpty | StratifiedParseTree {
    stratBody :: a,
    strata :: Int,
    stratChildren :: [StratifiedParseTree a]
} deriving (Show, Read, Eq)


type DepthZippedToken = (Token, Int)


generateDZTFromTokens :: [Token] -> [DepthZippedToken]
generateDZTFromTokens [] = []
generateDZTFromTokens ts = generateDZTFromTokens' ts 0 where
    generateDZTFromTokens' :: [Token] -> Int -> [DepthZippedToken]
    generateDZTFromTokens' [] _ = []
    generateDZTFromTokens' (x:xs) d
        | x `like` (Keyword Fish) && x /= (Keyword Hook)             = (x, d)       : generateDZTFromTokens' xs (d + 1)
        | any (x == ) [Bracket (Send Open), Bracket (Return Open)]   = (x, (d + 1)) : generateDZTFromTokens' xs (d + 1)
        | any (x == ) [Bracket (Send Close), Bracket (Return Close)] = (x, d)       : generateDZTFromTokens' xs (d - 1)
        | otherwise                                                  = (x, d)       : generateDZTFromTokens' xs d


unzipDZT :: [DepthZippedToken] -> [Token]
unzipDZT dzt = map fst dzt


removeDZTBrackets :: [DepthZippedToken] -> [DepthZippedToken]
removeDZTBrackets dzt = filter (\d -> not (like (fst d) (Bracket (Send Open)))) dzt


class TreeIO r where
    fPrintTree  :: (Show a) => Int -> r a -> String
    ioPrintTree :: (Show a) => r a -> IO ()


instance TreeIO ParseTree where
    fPrintTree d (ParseTree b cs) = (concat (replicate ((d * 4) - 1) "-")) ++ ">" ++ show b ++ "\n" ++ (concat (map (\c -> fPrintTree (d + 1) c) cs))
    ioPrintTree t = putStrLn $ fPrintTree 0 t


instance TreeIO StratifiedParseTree where
    fPrintTree d (StratifiedParseTree b depth cs) = (concat (replicate ((d * 4) - 1) "-")) ++ ">" ++ show b ++ "    " ++ show depth ++ "\n" ++ (concat (map (\c -> fPrintTree (d + 1) c) cs))
    ioPrintTree t = putStrLn $ fPrintTree 0 t


instance Functor ParseTree where
    fmap f (ParseTree b cs) = ParseTree (f b) (map (\c -> fmap f c) cs)


instance Functor StratifiedParseTree where
    fmap f (StratifiedParseTree b d cs) = StratifiedParseTree (f b) d (map (\c -> fmap f c) cs)


deStratifyTree :: StratifiedParseTree a -> ParseTree a
deStratifyTree StratEmpty = Empty
deStratifyTree (StratifiedParseTree b d cs) = ParseTree b (map deStratifyTree cs)


stratifyTree :: ParseTree a -> StratifiedParseTree a
stratifyTree pt = stratifyTree' pt 0 where
    stratifyTree' :: ParseTree a -> Int -> StratifiedParseTree a
    stratifyTree' Empty _ = StratEmpty
    stratifyTree' (ParseTree b cs) d = StratifiedParseTree b (d) (map (\t -> stratifyTree' t (d + 1)) cs)


getStrataList :: StratifiedParseTree a -> Int -> [StratifiedParseTree a]
getStrataList StratEmpty _ = []
getStrataList spt toDepth
    | toDepth == (strata spt) = spt : concat (map (\c -> getStrataList c toDepth) (stratChildren spt))
    | otherwise               = concat $ map (\c -> getStrataList c toDepth) (stratChildren spt)


-- sendBracketNC   = NCCase ((Bracket (Send Open)) ==) ((Bracket (Send Close)) ==)
bracketNC = NCCase (\x -> any (x ==) [Bracket (Send Open), Bracket (Return Open)]) (\x -> any (x ==) [Bracket (Send Close), Bracket (Return Close)])
-- returnBracketNC = NCCase ((Bracket (Return Open)) ==) ((Bracket (Return Close)) ==)


nullTree :: ParseTree a -> Bool
nullTree Empty = True
nullTree _     = False


tree :: a -> ParseTree a
tree x = ParseTree x []


-- append :: a -> ParseTree a -> ParseTree a
-- append x Empty            = tree x
-- append x (ParseTree b cs) = ParseTree b ((tree x) : cs)


appendChild :: ParseTree a -> ParseTree a -> ParseTree a
appendChild (ParseTree b cs) pt = ParseTree b (cs ++ [pt])


extendChildren :: ParseTree a -> [ParseTree a] -> ParseTree a
extendChildren (ParseTree b cs) pts = ParseTree b (cs ++ pts)


-- generateStratifiedParseTree :: [DepthZippedToken] -> StratifiedParseTree Token
-- generateStratifiedParseTree dzt = generateStratifiedParseTree' dzt 0 StratEmpty where
--     generateStratifiedParseTree' :: [DepthZippedToken] -> Int -> StratifiedParseTree Token -> StratifiedParseTree Token
--     generateStratifiedParseTree' [] _ spt = spt
--     generateStratifiedParseTree' (x:xs) depth StratEmpty = generateStratifiedParseTree' xs 1 (StratifiedParseTree (fst x) depth [])
--     generateStratifiedParseTree' (x:xs) depth spt
--         | (snd x) > depth  = appendChild spt (generateStratifiedParseTree' (x:xs) (depth + 1) StratEmpty)
--         | (snd x) == depth = 


-- fPrintTree :: (Show a) => Int -> ParseTree a -> String
-- fPrintTree d (ParseTree b cs) = (concat (replicate ((d * 4) - 1) "-")) ++ ">" ++ show b ++ "\n" ++ (concat (map (\c -> fPrintTree (d + 1) c) cs))


-- ioPrintTree :: (Show a) => ParseTree a -> IO ()
-- ioPrintTree t = putStrLn $ fPrintTree 0 t


generateParseTree :: [Token] -> ParseTree Token
generateParseTree [] = Empty
generateParseTree ts = generateParseTree' ts Empty where
    generateParseTree' :: [Token] -> ParseTree Token -> ParseTree Token
    generateParseTree' [] pt = pt
    generateParseTree' (t:ts) Empty = generateParseTree' ts (ParseTree t []) --This line could cause some Issues I think
    generateParseTree' (t:ts) pt
        -- | isSuperordinating t = pt
        -- | isSubordinating   t = appendChild pt (generateParseTree' (getNestedCollapsibleContents bracketNCCase (takeNestFirstComplete bracketNCCase (t:ts))) Empty)
        | otherwise           = extendChildren pt (map (\x -> ParseTree x []) (takeWhile (\x' -> not (endCase bracketNCCase x')) (t:ts)))
        where
            isSuperordinating :: Token -> Bool
            isSuperordinating t' = endCase bracketNCCase t'
            isSubordinating :: Token -> Bool
            isSubordinating t' = (t' `like` Keyword Fish && t' /= Keyword Hook) || beginCase bracketNCCase t'
            bracketNCCase :: NCCase Token
            bracketNCCase = NCCase (\x -> elem x [Bracket (Send Open), Bracket (Return Open)]) (\x -> elem x [Bracket (Send Close), Bracket (Return Close)])