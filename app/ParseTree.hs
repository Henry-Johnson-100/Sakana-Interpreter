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


nullTree :: ParseTree a -> Bool
nullTree Empty = True
nullTree _     = False


tree :: a -> ParseTree a
tree x = ParseTree x []


appendChild :: ParseTree a -> ParseTree a -> ParseTree a
appendChild (ParseTree b cs) pt = ParseTree b (cs ++ [pt])


extendChildren :: ParseTree a -> [ParseTree a] -> ParseTree a
extendChildren (ParseTree b cs) pts = ParseTree b (cs ++ pts)


generateParseTree :: [TokenUnit] -> ParseTree TokenUnit -> ParseTree TokenUnit
generateParseTree [] base = base
generateParseTree tus base 
    | any (\x -> (unit (head tus)) `like` x) [genericKeyword, genericControl, genericOperator] = appendChild base (generateParseTree (tail tus) (tree (head tus)))
    | nestedCollapsibleIsPrefixOf bracketNC tus                                                = generateParseTree (dropInfix (takeNestFirstComplete bracketNC tus) tus) (getBracketTree tus base)
    | otherwise                                                                                = generateParseTree (tail tus) (appendChild base (tree (head tus)))
    where
        getBracketTree :: [TokenUnit] -> ParseTree TokenUnit -> ParseTree TokenUnit
        getBracketTree tus base = if any (\x -> (Data (Punct ",")) == (unit x)) (takeNestFirstComplete bracketNC tus)
                                  then extendChildren base (map (\tus' -> generateParseTree tus' (tree (head tus))) (splitOn (\x -> (Data (Punct ",")) == (unit x)) (getNestedCollapsibleContents bracketNC (takeNestFirstComplete bracketNC tus))))
                                  else appendChild base (generateParseTree (getNestedCollapsibleContents bracketNC (takeNestFirstComplete bracketNC tus)) (tree (head tus)))