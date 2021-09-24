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
generateParseTree tus = collapseTreeListToHeadless $ map returnPartitionTree $ groupReturnPartitions tus

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
returnPartitionTree :: ReturnPartition -> ParseTree
returnPartitionTree (TriplePartition [] [] []) = Empty
returnPartitionTree (TriplePartition x [] []) = treeOnlyNonTerminals (TriplePartition x [] [])
returnPartitionTree (TriplePartition [] y []) = collapseTreeListToHeadless $ treeConcurrentBracketGroups y
returnPartitionTree (TriplePartition [] [] z) = treeOnlyValue (TriplePartition [] [] z)
returnPartitionTree (TriplePartition x [] z) = treeNoArgs (TriplePartition x [] z)
returnPartitionTree (TriplePartition x y []) = treeFunctionCall (TriplePartition x y [])
returnPartitionTree (TriplePartition [] y z) = treeAnonFunction (TriplePartition [] y z)
returnPartitionTree (TriplePartition x y z) = treeFullDeclaration (TriplePartition x y z)

treeOnlyNonTerminals :: ReturnPartition -> ParseTree
treeOnlyNonTerminals (TriplePartition x [] []) = serialTree x

treeOnlyValue :: ReturnPartition -> ParseTree
treeOnlyValue (TriplePartition [] [] z) = treeSingleBracketGroup z

treeNoArgs :: ReturnPartition -> ParseTree
treeNoArgs (TriplePartition x [] z) = liftHeadlessChildren $ declaration -<- funcReturn
  where
    declaration = serialTree x
    funcReturn = treeSingleBracketGroup z

treeFunctionCall :: ReturnPartition -> ParseTree
treeFunctionCall (TriplePartition x y []) = liftHeadlessChildren $ funcId -<- funcArgs
  where
    funcId = serialTree x
    funcArgs = collapseTreeListToHeadless $ treeConcurrentBracketGroups y

treeAnonFunction :: ReturnPartition -> ParseTree
treeAnonFunction (TriplePartition [] y z) = (Headless [] -<= treeChildren args) -<- funcReturn
  where
    funcReturn = treeSingleBracketGroup z
    args = collapseTreeListToHeadless $ treeConcurrentBracketGroups y

treeFullDeclaration :: ReturnPartition -> ParseTree
treeFullDeclaration (TriplePartition x y z) = liftHeadlessChildren $ (declaration -<= treeChildren args) -<- funcReturn
  where
    declaration = serialTree x
    funcReturn = treeSingleBracketGroup z
    args = collapseTreeListToHeadless $ treeConcurrentBracketGroups y

collapseTreeListToHeadless :: [ParseTree] -> ParseTree
collapseTreeListToHeadless [] = Empty
collapseTreeListToHeadless ptms = Headless ptms

treeConcurrentBracketGroups :: [TokenUnit] -> [ParseTree]
treeConcurrentBracketGroups tus = map treeSingleBracketGroup (groupBrackets tus)
  where
    groupBrackets :: [TokenUnit] -> [[TokenUnit]]
    groupBrackets [] = [[]]
    groupBrackets tus = groupAllTopLevelNestedCollapsibles bracketNC tus

treeSingleBracketGroup :: [TokenUnit] -> ParseTree
treeSingleBracketGroup [] = Empty
treeSingleBracketGroup xs
  | isCompleteNestedCollapsible bracketNC xs = treeSingleBracketGroup (takeNestWhileComplete bracketNC xs)
  | hasNestedCollapsible bracketNC xs = collapseTreeListToHeadless $ map returnPartitionTree $ groupReturnPartitions xs
  | otherwise = serialTree xs

liftHeadlessChildren :: ParseTree -> ParseTree
liftHeadlessChildren (Headless cs) = Headless cs
liftHeadlessChildren (n :-<-: cs) = tree n -<= concatMap (\c -> if isHeadless c then treeChildren c else [c]) cs