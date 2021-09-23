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
serialTree (x:xs) = tree x -<<- serialTree xs

treeNode :: Tree a -> a
treeNode (b :-<-: _) = b

treeChildren :: Tree a -> [Tree a]
treeChildren (_ :-<-: cs) = cs

(-<<-) :: Tree a -> Tree a -> Tree a
(b :-<-: cs) -<<- t = b :-<-: (cs ++ [t])

(-<<=) :: Tree a -> [Tree a] -> Tree a
(b :-<-: cs) -<<= ts = b :-<-: (cs ++ ts)

type ParseTree = Tree TokenUnit

newtype MonadicContainer a = MonadicContainer a deriving (Show, Eq)

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

groupReturnPartitions :: [TokenUnit] -> [ReturnPartition]
groupReturnPartitions [] = []
groupReturnPartitions tus = map partitionReturnGroup (breakReturnGroups tus) where
  partitionReturnGroup :: [TokenUnit] -> ReturnPartition
  partitionReturnGroup [] = TriplePartition [] [] []
  partitionReturnGroup tus = TriplePartition w a r where
    w = takeWhileList (not . nestedCollapsibleIsPrefixOf bracketNC) tus
    a = takeBracketNCExcludingReturn (dropInfix w tus)
    r = dropInfix (w ++ a) tus
  breakReturnGroups [] = []
  breakReturnGroups tus' = fst (breakScopeOnReturnGroup tus') : breakReturnGroups (snd (breakScopeOnReturnGroup tus')) where
    breakScopeOnReturnGroup :: [TokenUnit] -> ([TokenUnit], [TokenUnit])
    breakScopeOnReturnGroup [] = ([],[])
    breakScopeOnReturnGroup tus = (takenThroughReturn, dropInfix takenThroughReturn tus) where
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
putOnlyNonTerminals (TriplePartition x [] []) = do
  put (serialTree x)

putOnlyValue :: ReturnPartition -> ParseTreeMonad
putOnlyValue (TriplePartition [] [] z) = putSingleBracketGroup z

putAnonFunction :: ReturnPartition -> ParseTreeMonad
putAnonFunction (TriplePartition [] y z) = do
  declaration <- put (serialTree [PacketUnit (Keyword Fish) 0, PacketUnit (Data (Id "anon")) 0]) --idk what else to put here
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
collapseParseTreeMonadList [] = put (Empty::ParseTree)
collapseParseTreeMonadList ptms = put $ tree (PacketUnit (Data (Id "::headless::")) 0) -<<= map get ptms

putConcurrentBracketGroups :: [TokenUnit] -> [ParseTreeMonad]
putConcurrentBracketGroups tus = map putSingleBracketGroup (groupBrackets tus) where
  groupBrackets :: [TokenUnit] -> [[TokenUnit]]
  groupBrackets [] = [[]]
  groupBrackets tus = groupAllTopLevelNestedCollapsibles bracketNC tus

-- putSendGroup :: [TokenUnit] -> [ParseTreeMonad]
-- putSendGroup [] = [put (Empty::ParseTree)]
-- putSendGroup xs = put (trees xs)

putSingleBracketGroup :: [TokenUnit] -> ParseTreeMonad
putSingleBracketGroup [] = put (Empty::ParseTree)
putSingleBracketGroup xs
    | isCompleteNestedCollapsible bracketNC xs = putSingleBracketGroup (takeNestWhileComplete bracketNC xs)
    | hasNestedCollapsible bracketNC xs = collapseParseTreeMonadList $ map putReturnPartition $ groupReturnPartitions xs
    | otherwise = put (serialTree xs)

putUnnestedBracketContents :: [TokenUnit] -> ParseTreeMonad
putUnnestedBracketContents [] = put Empty
putUnnestedBracketContents tus = put (serialTree tus)

-- bracketSUNC :: NCCase SyntaxUnit
-- bracketSUNC = NCCase (\x -> syntaxToken x `elem` [Bracket Send Open, Bracket Return Open]) (\x -> syntaxToken x `elem` [Bracket Send Close, Bracket Return Close])



tokenUnitIsComma :: TokenUnit -> Bool
tokenUnitIsComma (PacketUnit (Data (Punct ",")) _) = True
tokenUnitIsComma _                                 = False

isSubordinator :: TokenUnit -> Bool
isSubordinator tu = unit tu `like` genericKeyword || unit tu `like` genericOperator || dataTokenIsId (unit tu)

-- takeSubordinatorGroup :: [TokenUnit] -> [TokenUnit]
-- takeSubordinatorGroup [] = []
-- takeSubordinatorGroup (tu:tus)
--     | unit tu == Keyword Migrate    = partFst part ++ partSnd part
--     | unit tu `like` genericKeyword = takeBracketNCIncludingReturn (tu:tus)
--     | otherwise                     = takeBracketNCExcludingReturn (tu:tus)
--     where
--       part = breakByNest bracketNC (tu:tus)

takeBracketNCExcludingReturn :: [TokenUnit] -> [TokenUnit]
takeBracketNCExcludingReturn [] = []
takeBracketNCExcludingReturn (tu:tus)
    | null (partSnd part) = []
    | getTokenBracketScopeType (unit (head (partSnd part))) == Return = []
    | otherwise                                                       = partFstSnd ++ takeBracketNCExcludingReturn (partThd part)
    where
      part = breakByNest bracketNC (tu:tus)
      partFstSnd = partFst part ++ partSnd part

takeBracketNCIncludingReturn :: [TokenUnit] -> [TokenUnit]
takeBracketNCIncludingReturn [] = []
takeBracketNCIncludingReturn (tu:tus)
    | null (partSnd part) = []
    | getTokenBracketScopeType (unit (head (partSnd part))) == Return = partFstSnd
    | otherwise                                                       = partFstSnd ++ takeBracketNCIncludingReturn (partThd part)
    where
      part = breakByNest bracketNC (tu:tus)
      partFstSnd = partFst part ++ partSnd part

groupUninterruptedBrackets :: [TokenUnit] -> [[TokenUnit]]
groupUninterruptedBrackets [] = []
groupUninterruptedBrackets tus
    | not (null (partFst part)) = []
    | otherwise                 = partSnd part : groupUninterruptedBrackets (partThd part)
    where
      part = breakByNest bracketNC tus

-- getCompleteBracketNCArguments :: [TokenUnit] -> [[TokenUnit]]
-- getCompleteBracketNCArguments [] = [[]]
-- getCompleteBracketNCArguments tus = if any tokenUnitIsComma tus then splitTopLevelNCOn bracketNC tokenUnitIsComma tus else [tus]

-- generateParseTree :: [TokenUnit] -> ParseTree
-- generateParseTree tus = topLevelGroupTree tus Return (tree (PacketUnit (Data (Id "main")) 0))

-- topLevelGroupTree :: [TokenUnit] -> ScopeType -> ParseTree -> ParseTree
-- topLevelGroupTree [] st parent = parent
-- topLevelGroupTree (tu:tus) st parent
--     | nestedCollapsibleIsPrefixOf bracketNC (tu:tus) = (-<|=) parent (map bracketTree bracketParallelGroups) st
--     | otherwise                                      = (-<|-) parent (topLevelGroupTree tus st (tree tu)) st
--     where
--       bracketParallelGroups = groupUninterruptedBrackets (tu:tus)

-- bracketTree :: [TokenUnit] -> ParseTree
-- bracketTree tus = makeHeadlessTree (getNestedCollapsibleContents bracketNC tus) (getTokenBracketScopeType (unit (head tus)))

-- makeHeadlessTree :: [TokenUnit] -> ScopeType -> ParseTree
-- makeHeadlessTree [] _ = Empty
-- makeHeadlessTree tus st = topLevelGroupTree (tail tus) st (tree (head tus))

-- topLevelGroupTree :: [TokenUnit] -> ParseTree -> ScopeType -> ParseTree
-- topLevelGroupTree [] parent _ = parent
-- topLevelGroupTree (tu:tus) parent st
--     | isSubordinator tu = topLevelGroupTree (dropInfix takenSubordinatedGroup (tu:tus)) (parent -<|- makeHeadlessTree takenSubordinatedGroup st) st
--     | otherwise         = topLevelGroupTree (dropInfix partFstSnd (tu:tus)) (parent -<|- makeHeadlessTree partFstSnd st) st
--     where
--     (-<|-) :: ParseTree -> ParseTree -> ParseTree
--     parent' -<|- child' = if st == Send then parent' -<.- child' else parent' -<*- child'
--     (-<|=) :: ParseTree -> [ParseTree] -> ParseTree
--     parent' -<|= [] = parent'
--     parent' -<|= [child'] = parent' -<|- child'
--     parent' -<|= (child' : children') = (parent' -<|- child') -<|= children'
--     bracketPartition = breakByNest bracketNC (tu : tus)
--     prefixedNC = takeNestFirstComplete bracketNC (tu:tus)
--     takenSubordinatedGroup = takeSubordinatorGroup (tu:tus)
--     part = breakByNest bracketNC (tu:tus)
--     partFstSnd = partFst part ++ partSnd part

-- -- |Works pretty well for single, unnested functions
-- distinctGroupTree :: [TokenUnit] -> ParseTree -> ScopeType -> ParseTree
-- distinctGroupTree [] parent _ = parent
-- distinctGroupTree (tu' : tus') parent st
--   | isSubordinator tu' = parent -<|- distinctGroupTree tus' (tree tu') st
--   | nestedCollapsibleIsPrefixOf bracketNC (tu' : tus') = distinctGroupTree (dropInfix prefixedNC (tu':tus')) (parent -<|= map (`makeHeadlessTree` st) (getCompleteBracketNCArguments (getNestedCollapsibleContents bracketNC prefixedNC))) (getTokenBracketScopeType (unit tu')) --This line is the issue right now
--   | otherwise = distinctGroupTree tus' (parent -<|- tree tu') st
--   where
--     (-<|-) :: ParseTree -> ParseTree -> ParseTree
--     parent' -<|- child' = if st == Send then parent' -<.- child' else parent' -<*- child'
--     (-<|=) :: ParseTree -> [ParseTree] -> ParseTree
--     parent' -<|= [] = parent'
--     parent' -<|= [child'] = parent' -<|- child'
--     parent' -<|= (child' : children') = (parent' -<|- child') -<|= children'
--     bracketPartition = breakByNest bracketNC (tu' : tus')
--     prefixedNC = takeNestFirstComplete bracketNC (tu':tus')