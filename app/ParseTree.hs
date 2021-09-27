module ParseTree
  ( generateParseTree,
    TreeIO (..),
  )
where

import Data.List (mapAccumL)
import Lexer
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

data SyntaxUnit = SyntaxUnit
  { token :: Token,
    line :: Int,
    context :: ScopeType
  }
  deriving (Show, Eq)

type SyntaxTree = Tree SyntaxUnit

type SyntaxPartition = TriplePartition SyntaxUnit

generateParseTree :: [TokenUnit] -> SyntaxTree
generateParseTree [] = Empty
generateParseTree tus = collapseTreeListToHeadless $ map syntaxChunkTree $ (groupSyntaxChunks . scanTokensToSyntaxes) tus

bracketNestCase :: NCCase SyntaxUnit
bracketNestCase = NCCase (\x -> token x `elem` [Bracket Send Open, Bracket Return Open]) (\x -> token x `elem` [Bracket Send Close, Bracket Return Close])

takeBracketNCExcludingReturn :: [SyntaxUnit] -> [SyntaxUnit]
takeBracketNCExcludingReturn [] = []
takeBracketNCExcludingReturn (tu : tus)
  | null (partSnd part) = []
  | getTokenBracketScopeType (token (head (partSnd part))) == Return = []
  | otherwise = partFstSnd ++ takeBracketNCExcludingReturn (partThd part)
  where
    part = breakByNest bracketNestCase (tu : tus)
    partFstSnd = partFst part ++ partSnd part

takeBracketNCIncludingReturn :: [SyntaxUnit] -> [SyntaxUnit]
takeBracketNCIncludingReturn [] = []
takeBracketNCIncludingReturn (tu : tus)
  | null (partSnd part) = []
  | getTokenBracketScopeType (token (head (partSnd part))) == Return = partFstSnd
  | otherwise = partFstSnd ++ takeBracketNCIncludingReturn (partThd part)
  where
    part = breakByNest bracketNestCase (tu : tus)
    partFstSnd = partFst part ++ partSnd part

scanTokensToSyntaxes :: [TokenUnit] -> [SyntaxUnit]
scanTokensToSyntaxes [] = []
scanTokensToSyntaxes tus = zipWith tokenUnitToSyntaxUnit tus (scanScopeTypes tus)
  where
    tokenUnitToSyntaxUnit :: TokenUnit -> ScopeType -> SyntaxUnit
    tokenUnitToSyntaxUnit tu = SyntaxUnit (unit tu) (unitLine tu)
    scanScopeTypes :: [TokenUnit] -> [ScopeType]
    scanScopeTypes [] = []
    scanScopeTypes tus = scanl getScanScopeType Return tus
      where
        getScanScopeType :: ScopeType -> TokenUnit -> ScopeType
        getScanScopeType _ (PacketUnit (Bracket Send Open) _) = Send
        getScanScopeType _ (PacketUnit (Bracket Return Open) _) = Return
        getScanScopeType st _ = st

groupSyntaxChunks :: [SyntaxUnit] -> [SyntaxPartition]
groupSyntaxChunks [] = []
groupSyntaxChunks tus = map partitionSyntaxChunk (breakSyntaxChunk tus)
  where
    partitionSyntaxChunk :: [SyntaxUnit] -> SyntaxPartition
    partitionSyntaxChunk [] = TriplePartition [] [] []
    partitionSyntaxChunk tus = TriplePartition w a r
      where
        w = takeWhileList (not . nestedCollapsibleIsPrefixOf bracketNestCase) tus
        a = takeBracketNCExcludingReturn (dropInfix w tus)
        r = dropInfix (w ++ a) tus
    breakSyntaxChunk :: [SyntaxUnit] -> [[SyntaxUnit]]
    breakSyntaxChunk [] = []
    breakSyntaxChunk tus' = fst (breakScopeOnSyntaxChunk tus') : breakSyntaxChunk (snd (breakScopeOnSyntaxChunk tus'))
      where
        breakScopeOnSyntaxChunk :: [SyntaxUnit] -> ([SyntaxUnit], [SyntaxUnit])
        breakScopeOnSyntaxChunk [] = ([], [])
        breakScopeOnSyntaxChunk tus = (takenThroughReturn, dropInfix takenThroughReturn tus)
          where
            takenThroughReturn = takeBracketNCIncludingReturn tus

-- | Receptacle for all possible pattern matches of a TriplePartition when making a tree
syntaxChunkTree :: SyntaxPartition -> SyntaxTree
syntaxChunkTree (TriplePartition [] [] []) = Empty
syntaxChunkTree (TriplePartition x [] []) = treeOnlyNonTerminals (TriplePartition x [] [])
syntaxChunkTree (TriplePartition [] y []) = (collapseTreeListToHeadless . treeConcurrentBracketGroups) y
syntaxChunkTree (TriplePartition [] [] z) = treeOnlyValue (TriplePartition [] [] z)
syntaxChunkTree (TriplePartition x [] z) = treeNoArgs (TriplePartition x [] z)
syntaxChunkTree (TriplePartition x y []) = treeFunctionCall (TriplePartition x y [])
syntaxChunkTree (TriplePartition [] y z) = treeAnonFunction (TriplePartition [] y z)
syntaxChunkTree (TriplePartition x y z) = treeFullDeclaration (TriplePartition x y z)

treeOnlyNonTerminals :: SyntaxPartition -> SyntaxTree
treeOnlyNonTerminals (TriplePartition x [] []) = serialTree x

treeOnlyValue :: SyntaxPartition -> SyntaxTree
treeOnlyValue (TriplePartition [] [] z) = treeSingleBracketGroup z

treeNoArgs :: SyntaxPartition -> SyntaxTree
treeNoArgs (TriplePartition x [] z) = liftHeadlessChildren $ declaration -<- funcReturn
  where
    declaration = serialTree x
    funcReturn = treeSingleBracketGroup z

treeFunctionCall :: SyntaxPartition -> SyntaxTree
treeFunctionCall (TriplePartition x y []) = liftHeadlessChildren $ funcId -<- funcArgs
  where
    funcId = serialTree x
    funcArgs = collapseTreeListToHeadless $ treeConcurrentBracketGroups y

treeAnonFunction :: SyntaxPartition -> SyntaxTree
treeAnonFunction (TriplePartition [] y z) = (Headless [] -<= treeChildren args) -<- funcReturn
  where
    funcReturn = treeSingleBracketGroup z
    args = collapseTreeListToHeadless $ treeConcurrentBracketGroups y

treeFullDeclaration :: SyntaxPartition -> SyntaxTree
treeFullDeclaration (TriplePartition x y z) = liftHeadlessChildren $ (declaration -<= treeChildren args) -<- funcReturn
  where
    declaration = serialTree x
    funcReturn = treeSingleBracketGroup z
    args = collapseTreeListToHeadless $ treeConcurrentBracketGroups y

collapseTreeListToHeadless :: [SyntaxTree] -> SyntaxTree
collapseTreeListToHeadless [] = Empty
collapseTreeListToHeadless ptms = Headless ptms

treeConcurrentBracketGroups :: [SyntaxUnit] -> [SyntaxTree]
treeConcurrentBracketGroups tus = map treeSingleBracketGroup (groupBrackets tus)
  where
    groupBrackets :: [SyntaxUnit] -> [[SyntaxUnit]]
    groupBrackets [] = [[]]
    groupBrackets tus = groupAllTopLevelNestedCollapsibles bracketNestCase tus

treeSingleBracketGroup :: [SyntaxUnit] -> SyntaxTree
treeSingleBracketGroup [] = Empty
treeSingleBracketGroup xs
  | isCompleteNestedCollapsible bracketNestCase xs = treeSingleBracketGroup (takeNestWhileComplete bracketNestCase xs)
  | hasNestedCollapsible bracketNestCase xs = collapseTreeListToHeadless $ map syntaxChunkTree $ groupSyntaxChunks xs
  | otherwise = serialTree xs

liftHeadlessChildren :: SyntaxTree -> SyntaxTree
liftHeadlessChildren (Headless cs) = Headless cs
liftHeadlessChildren (n :-<-: cs) = tree n -<= concatMap (\c -> if isHeadless c then treeChildren c else [c]) cs