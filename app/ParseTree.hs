module ParseTree(
ParseTree(..),
generateParseTree
) where

import Lexer
import Token.Bracket
import Token.Control
import Token.Data
import Token.Keyword
import Token.Operator
import Token.Util.NestedCollapsible
import Data.List

data ParseTree a = Empty | ParseTree {
    body     :: a,
    children :: [ParseTree a]
} deriving (Show, Read, Eq)


instance Functor ParseTree where
    fmap f (ParseTree b cs) = ParseTree (f b) (map (\c -> fmap f c) cs)


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


fPrintTree :: (Show a) => Int -> ParseTree a -> String
fPrintTree d (ParseTree b cs) = (concat (replicate ((d * 4) - 1) "-")) ++ ">" ++ show b ++ "\n" ++ (concat (map (\c -> fPrintTree (d + 1) c) cs))


ioPrintTree :: (Show a) => ParseTree a -> IO ()
ioPrintTree t = putStrLn $ fPrintTree 0 t


generateParseTree :: [Token] -> ParseTree Token
generateParseTree [] = Empty
generateParseTree ts = generateParseTree' Empty ts where
    generateParseTree' :: ParseTree Token -> [Token] -> ParseTree Token
    generateParseTree' pt [] = pt
    generateParseTree' Empty (t:ts) = generateParseTree' (tree t) ts
    generateParseTree' pt ts
        | isBracketPrefixed ts = generateParseTree' (extendChildren pt ()) (partThd (part ts))
        where
            isBracketPrefixed ts' = nestedCollapsibleIsPrefixOf bracketNC ts'
            part ts' = breakByNest bracketNC ts'