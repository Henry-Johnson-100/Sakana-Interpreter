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


nullTree :: ParseTree a -> Bool
nullTree Empty = True
nullTree _     = False


tree :: a -> ParseTree a
tree x = ParseTree x []


append :: a -> ParseTree a -> ParseTree a
append x Empty            = tree x
append x (ParseTree b cs) = ParseTree b ((tree x) : cs)


fPrintTree :: (Show a) => Int -> ParseTree a -> String
fPrintTree d (ParseTree b cs) = (concat (replicate ((d * 4) - 1) "-")) ++ ">" ++ show b ++ "\n" ++ (concat (map (\c -> fPrintTree (d + 1) c) cs))


ioPrintTree :: (Show a) => ParseTree a -> IO ()
ioPrintTree t = putStrLn $ fPrintTree 0 t


generateParseTree :: [Token] -> ParseTree Token
generateParseTree ts = Empty