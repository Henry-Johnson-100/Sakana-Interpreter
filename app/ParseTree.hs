module ParseTree
  ( generateParseTree,
    TreeIO (..),
  )
where

import Lexer
  ( PacketUnit (unit),
    Token (Bracket),
    TokenUnit,
    getTokenBracketScopeType,
  )
import Token.Bracket
  ( BracketTerminal (Close, Open),
    ScopeType (Return, Send),
  )
import Token.Util.EagerCollapsible (dropInfix)
import Token.Util.NestedCollapsible
  ( NCCase (NCCase),
    TriplePartition (..),
    breakByNest,
    groupAllTopLevelNestedCollapsibles,
    hasNestedCollapsible,
    isCompleteNestedCollapsible,
    nestedCollapsibleIsPrefixOf,
    takeNestWhileComplete,
    takeWhileList,
  )
import Token.Util.Tree
  ( Tree (..),
    TreeIO (..),
    isHeadless,
    serialTree,
    tree,
    treeChildren,
    (-<-),
    (-<=),
  )

type ParseTree = Tree TokenUnit

type ReturnPartition = TriplePartition TokenUnit

generateParseTree :: [TokenUnit] -> ParseTree
generateParseTree [] = Empty
generateParseTree tus = collapseParseTreeMonadList $ map putReturnPartition $ groupReturnPartitions tus

bracketNC :: NCCase TokenUnit
bracketNC = NCCase (\x -> unit x `elem` [Bracket Send Open, Bracket Return Open]) (\x -> unit x `elem` [Bracket Send Close, Bracket Return Close])

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
putReturnPartition :: ReturnPartition -> ParseTree
putReturnPartition (TriplePartition [] [] []) = Empty
putReturnPartition (TriplePartition x [] []) = putOnlyNonTerminals (TriplePartition x [] [])
putReturnPartition (TriplePartition [] y []) = collapseParseTreeMonadList $ putConcurrentBracketGroups y
putReturnPartition (TriplePartition [] [] z) = putOnlyValue (TriplePartition [] [] z)
putReturnPartition (TriplePartition x [] z) = putNoArgs (TriplePartition x [] z)
putReturnPartition (TriplePartition x y []) = putFunctionCall (TriplePartition x y [])
putReturnPartition (TriplePartition [] y z) = putAnonFunction (TriplePartition [] y z)
putReturnPartition (TriplePartition x y z) = putFullDeclaration (TriplePartition x y z)

putOnlyNonTerminals :: ReturnPartition -> ParseTree
putOnlyNonTerminals (TriplePartition x [] []) = serialTree x

putOnlyValue :: ReturnPartition -> ParseTree
putOnlyValue (TriplePartition [] [] z) = putSingleBracketGroup z

putNoArgs :: ReturnPartition -> ParseTree
putNoArgs (TriplePartition x [] z) = putLiftHeadlessChildren $ declaration -<- funcReturn
  where
    declaration = serialTree x
    funcReturn = putSingleBracketGroup z

putFunctionCall :: ReturnPartition -> ParseTree
putFunctionCall (TriplePartition x y []) = putLiftHeadlessChildren $ funcId -<- funcArgs
  where
    funcId = serialTree x
    funcArgs = collapseParseTreeMonadList $ putConcurrentBracketGroups y

putAnonFunction :: ReturnPartition -> ParseTree
putAnonFunction (TriplePartition [] y z) = (Headless [] -<= treeChildren args) -<- funcReturn
  where
    funcReturn = putSingleBracketGroup z
    args = collapseParseTreeMonadList $ putConcurrentBracketGroups y

putFullDeclaration :: ReturnPartition -> ParseTree
putFullDeclaration (TriplePartition x y z) = putLiftHeadlessChildren $ (declaration -<= treeChildren args) -<- funcReturn
  where
    declaration = serialTree x
    funcReturn = putSingleBracketGroup z
    args = collapseParseTreeMonadList $ putConcurrentBracketGroups y

collapseParseTreeMonadList :: [ParseTree] -> ParseTree
collapseParseTreeMonadList [] = Empty
collapseParseTreeMonadList ptms = Headless ptms

putConcurrentBracketGroups :: [TokenUnit] -> [ParseTree]
putConcurrentBracketGroups tus = map putSingleBracketGroup (groupBrackets tus)
  where
    groupBrackets :: [TokenUnit] -> [[TokenUnit]]
    groupBrackets [] = [[]]
    groupBrackets tus = groupAllTopLevelNestedCollapsibles bracketNC tus

putSingleBracketGroup :: [TokenUnit] -> ParseTree
putSingleBracketGroup [] = Empty
putSingleBracketGroup xs
  | isCompleteNestedCollapsible bracketNC xs = putSingleBracketGroup (takeNestWhileComplete bracketNC xs)
  | hasNestedCollapsible bracketNC xs = collapseParseTreeMonadList $ map putReturnPartition $ groupReturnPartitions xs
  | otherwise = serialTree xs

putLiftHeadlessChildren :: ParseTree -> ParseTree
putLiftHeadlessChildren = liftHeadlessChildren'
  where
    liftHeadlessChildren' :: ParseTree -> ParseTree
    liftHeadlessChildren' (Headless cs) = Headless cs
    liftHeadlessChildren' (n :-<-: cs) = tree n -<= concatMap (\c -> if isHeadless c then treeChildren c else [c]) cs