module ParseTree
  ( generateParseTree,
    TreeIO (..),
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

data Tree a = Empty | a :-<-: [Tree a] deriving (Show, Eq)

instance TreeIO Tree where
  fPrintTree d Empty = concat (replicate (d * 4 - 1) "-") ++ ">" ++ "Empty\n"
  fPrintTree d (n :-<-: a) = concat (replicate (d * 4 - 1) "-") ++ ">" ++ show n ++ "\n" ++ concatMap (fPrintTree (d + 1)) a
  ioPrintTree t = putStrLn $ fPrintTree 0 t

tree :: a -> Tree a
tree x = x :-<-: []

trees :: [a] -> [Tree a]
trees = map (:-<-: [])

serialTree :: [a] -> Tree a
serialTree [] = Empty
serialTree [x] = tree x
serialTree (x : xs) = tree x -<<- serialTree xs

treeNode :: Tree a -> a
treeNode (b :-<-: _) = b

treeChildren :: Tree a -> [Tree a]
treeChildren (_ :-<-: cs) = cs

transplantChildren :: Tree a -> Tree a -> Tree a
transplantChildren t (_ :-<-: cs) = t -<<= cs

(-<<-) :: Tree a -> Tree a -> Tree a
(b :-<-: cs) -<<- t = b :-<-: (cs ++ [t])

(-<<=) :: Tree a -> [Tree a] -> Tree a
(b :-<-: cs) -<<= ts = b :-<-: (cs ++ ts)

type ParseTree = Tree TokenUnit

newtype MonadicContainer a = MonadicContainer a deriving (Show, Eq)

instance Functor Tree where
  fmap f (b :-<-: cs) = f b :-<-: map (fmap f) cs

instance Functor MonadicContainer where
  fmap f (MonadicContainer x) = MonadicContainer (f x)

instance Applicative MonadicContainer where
  pure = MonadicContainer
  (MonadicContainer f) <*> (MonadicContainer x) = MonadicContainer (f x)

instance Monad MonadicContainer where
  return = MonadicContainer
  (MonadicContainer x) >>= f = f x

type ParseTreeMonad = MonadicContainer ParseTree

type ReturnPartition = TriplePartition TokenUnit

put :: a -> MonadicContainer a
put = MonadicContainer

get :: MonadicContainer a -> a
get (MonadicContainer x) = x

generateParseTree :: [TokenUnit] -> ParseTree
generateParseTree [] = Empty
generateParseTree tus = get $ collapseParseTreeMonadList $ map putReturnPartition $ groupReturnPartitions tus

filterHeadlessChildren :: ParseTree -> ParseTree
filterHeadlessChildren t = t -<<= map (\x -> if unitLine (treeNode x) == -1 then transplantChildren t x else x) (treeChildren t)

takeBracketNCExcludingReturn :: [TokenUnit] -> [TokenUnit]
takeBracketNCExcludingReturn [] = []
takeBracketNCExcludingReturn (tu : tus)
  | null (partSnd part) = []
  | getTokenBracketScopeType (unit (head (partSnd part))) == Return = []
  | otherwise = partFstSnd ++ takeBracketNCExcludingReturn (partThd part)
  where
    part = breakByNest bracketNC (tu : tus)
    partFstSnd = partFst part ++ partSnd part

takeBracketNCIncludingReturn :: [TokenUnit] -> [TokenUnit]
takeBracketNCIncludingReturn [] = []
takeBracketNCIncludingReturn (tu : tus)
  | null (partSnd part) = []
  | getTokenBracketScopeType (unit (head (partSnd part))) == Return = partFstSnd
  | otherwise = partFstSnd ++ takeBracketNCIncludingReturn (partThd part)
  where
    part = breakByNest bracketNC (tu : tus)
    partFstSnd = partFst part ++ partSnd part

groupReturnPartitions :: [TokenUnit] -> [ReturnPartition]
groupReturnPartitions [] = []
groupReturnPartitions tus = map partitionReturnGroup (breakReturnGroups tus)
  where
    partitionReturnGroup :: [TokenUnit] -> ReturnPartition
    partitionReturnGroup [] = TriplePartition [] [] []
    partitionReturnGroup tus = TriplePartition w a r
      where
        w = takeWhileList (not . nestedCollapsibleIsPrefixOf bracketNC) tus
        a = takeBracketNCExcludingReturn (dropInfix w tus)
        r = dropInfix (w ++ a) tus
    breakReturnGroups [] = []
    breakReturnGroups tus' = fst (breakScopeOnReturnGroup tus') : breakReturnGroups (snd (breakScopeOnReturnGroup tus'))
      where
        breakScopeOnReturnGroup :: [TokenUnit] -> ([TokenUnit], [TokenUnit])
        breakScopeOnReturnGroup [] = ([], [])
        breakScopeOnReturnGroup tus = (takenThroughReturn, dropInfix takenThroughReturn tus)
          where
            takenThroughReturn = takeBracketNCIncludingReturn tus

-- | Receptacle for all possible pattern matches of a TriplePartition when making a tree
putReturnPartition :: ReturnPartition -> ParseTreeMonad
putReturnPartition (TriplePartition [] [] []) = put                 (Empty::ParseTree)
putReturnPartition (TriplePartition x  [] []) = putOnlyNonTerminals (TriplePartition x [] [])
putReturnPartition (TriplePartition [] y  []) = collapseParseTreeMonadList $ putConcurrentBracketGroups y
putReturnPartition (TriplePartition [] [] z ) = putOnlyValue        (TriplePartition [] [] z)
putReturnPartition (TriplePartition x  [] z ) = putNoArgs           (TriplePartition x [] z)
putReturnPartition (TriplePartition x  y  []) = putFunctionCall     (TriplePartition x y [])
putReturnPartition (TriplePartition [] y  z ) = putAnonFunction     (TriplePartition [] y z)
putReturnPartition (TriplePartition x  y  z ) = putFullDeclaration  (TriplePartition x y z)

putFullDeclaration :: ReturnPartition -> ParseTreeMonad
putFullDeclaration (TriplePartition x y z) = do
  declaration <- put (serialTree x)
  funcReturn <- putSingleBracketGroup z
  args <- collapseParseTreeMonadList $ putConcurrentBracketGroups y
  put $ (declaration -<<= treeChildren args) -<<- funcReturn

putFunctionCall :: ReturnPartition -> ParseTreeMonad
putFunctionCall (TriplePartition x y []) = do
  funcId <- put (serialTree x)
  funcArgs <- collapseParseTreeMonadList $ putConcurrentBracketGroups y
  put $ funcId -<<- funcArgs

putOnlyNonTerminals :: ReturnPartition -> ParseTreeMonad
putOnlyNonTerminals (TriplePartition x [] []) = put (serialTree x)

putOnlyValue :: ReturnPartition -> ParseTreeMonad
putOnlyValue (TriplePartition [] [] z) = putSingleBracketGroup z

putAnonFunction :: ReturnPartition -> ParseTreeMonad
putAnonFunction (TriplePartition [] y z) = do
  declaration <- put (serialTree [PacketUnit (Keyword Fish) 0, PacketUnit (Data (Id "::anon::")) (-1)]) --idk what else to put here
  funcReturn <- putSingleBracketGroup z
  args <- collapseParseTreeMonadList $ putConcurrentBracketGroups y
  put $ (declaration -<<= treeChildren args) -<<- funcReturn

putNoArgs :: ReturnPartition -> ParseTreeMonad
putNoArgs (TriplePartition x [] z) = do
  declaration <- put (serialTree x)
  funcReturn <- putSingleBracketGroup z
  put $ declaration -<<- funcReturn

bracketNC :: NCCase TokenUnit
bracketNC = NCCase (\x -> unit x `elem` [Bracket Send Open, Bracket Return Open]) (\x -> unit x `elem` [Bracket Send Close, Bracket Return Close])

bracketReturnNC :: NCCase TokenUnit
bracketReturnNC = NCCase (\x -> unit x == Bracket Return Open) (\x -> unit x == Bracket Return Close)

collapseParseTreeMonadList :: [ParseTreeMonad] -> ParseTreeMonad
collapseParseTreeMonadList [] = put (Empty :: ParseTree)
collapseParseTreeMonadList ptms = put $ tree (PacketUnit (Data (Id "::headless::")) (-1)) -<<= map get ptms

putConcurrentBracketGroups :: [TokenUnit] -> [ParseTreeMonad]
putConcurrentBracketGroups tus = map putSingleBracketGroup (groupBrackets tus)
  where
    groupBrackets :: [TokenUnit] -> [[TokenUnit]]
    groupBrackets [] = [[]]
    groupBrackets tus = groupAllTopLevelNestedCollapsibles bracketNC tus

putSingleBracketGroup :: [TokenUnit] -> ParseTreeMonad
putSingleBracketGroup [] = put (Empty :: ParseTree)
putSingleBracketGroup xs
  | isCompleteNestedCollapsible bracketNC xs = putSingleBracketGroup (takeNestWhileComplete bracketNC xs)
  | hasNestedCollapsible bracketNC xs = collapseParseTreeMonadList $ map putReturnPartition $ groupReturnPartitions xs
  | otherwise = put (serialTree xs)

putUnnestedBracketContents :: [TokenUnit] -> ParseTreeMonad
putUnnestedBracketContents [] = put Empty
putUnnestedBracketContents tus = put (serialTree tus)
