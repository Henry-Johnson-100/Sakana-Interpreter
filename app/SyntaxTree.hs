module SyntaxTree
  ( generateSyntaxTree,
    generateModuleTree,
    SyntaxTree,
    TreeIO (..),
    SyntaxUnit (..),
  )
where

import Data.List (mapAccumL)
import Data.Maybe (fromJust, isNothing)
import Exception.Base
import Lexer
import Token.Bracket
  ( BracketTerminal (Close, Open),
    ScopeType (Return, Send),
  )
import Token.Data (Data (Id))
import Token.Keyword
import Token.Util.EagerCollapsible (dropInfix)
import Token.Util.Like
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

data SyntaxUnit = SyntaxUnit
  { token :: Token,
    line :: Int,
    context :: ScopeType
  }
  deriving (Show, Eq)

genericSyntaxUnit :: Token -> SyntaxUnit
genericSyntaxUnit t = SyntaxUnit t 0 Return

setContext :: SyntaxUnit -> ScopeType -> SyntaxUnit
setContext su st = su {context = st}

maybeOnTreeNode :: a -> (SyntaxUnit -> a) -> SyntaxTree -> a
maybeOnTreeNode defaultVal f st = maybe defaultVal f (treeNode st)

type SyntaxTree = Tree SyntaxUnit

type SyntaxPartition = TriplePartition SyntaxUnit

generateModuleTree :: String -> [TokenUnit] -> SyntaxTree
generateModuleTree name = flip nameModuleTree name . generateSyntaxTree

nameModuleTree :: SyntaxTree -> String -> SyntaxTree
nameModuleTree tr str = mutateTreeNode tr (\_ -> genericSyntaxUnit (Data (Id str)))

generateSyntaxTree :: [TokenUnit] -> SyntaxTree
generateSyntaxTree [] = Empty
generateSyntaxTree tus = reContextualizeSchoolMethods $ tree (genericSyntaxUnit (Data (Id "main"))) -<= concatMap syntaxChunkTree ((groupSyntaxChunks . scanTokensToSyntaxes) tus)

reContextualizeSchoolMethods :: SyntaxTree -> SyntaxTree
reContextualizeSchoolMethods Empty = Empty
reContextualizeSchoolMethods st
  | null (lookupOn st (\x -> (token . fromJust . treeNode) x == Keyword School)) = st
  | (token . fromJust . treeNode) st /= Keyword School = reTree st -<= childMap reContextualizeSchoolMethods st
  | otherwise = reTree st -<= map reContextualizeSchoolMethods reContextualizedChildren
  where
    reContextualizedChildren :: [SyntaxTree]
    reContextualizedChildren = fst breakOnSendReturn ++ map (\x -> mutateTreeNode x (`setContext` Return)) (snd breakOnSendReturn)
    breakOnSendReturn = span (\x -> Send == (context . fromJust . treeNode) x) (treeChildren st)

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
syntaxChunkTree :: SyntaxPartition -> [SyntaxTree]
syntaxChunkTree (TriplePartition [] [] []) = []
syntaxChunkTree (TriplePartition x [] []) = treeOnlyNonTerminals (TriplePartition x [] [])
syntaxChunkTree (TriplePartition [] y []) = treeConcurrentBracketGroups y
syntaxChunkTree (TriplePartition [] [] z) = treeOnlyValue (TriplePartition [] [] z)
syntaxChunkTree (TriplePartition x [] z) = treeNoArgs (TriplePartition x [] z)
syntaxChunkTree (TriplePartition x y []) = treeFunctionCall (TriplePartition x y [])
syntaxChunkTree (TriplePartition [] y z) = treeAnonFunction (TriplePartition [] y z)
syntaxChunkTree (TriplePartition x y z) = treeFullDeclaration (TriplePartition x y z)

treeOnlyNonTerminals :: SyntaxPartition -> [SyntaxTree]
treeOnlyNonTerminals (TriplePartition x [] []) = [serialTree x]

treeOnlyValue :: SyntaxPartition -> [SyntaxTree]
treeOnlyValue (TriplePartition [] [] z) = treeSingleBracketGroup z

treeNoArgs :: SyntaxPartition -> [SyntaxTree]
treeNoArgs (TriplePartition x [] z) = [declaration -<= funcReturn]
  where
    declaration = serialTree x
    funcReturn = treeSingleBracketGroup z

treeFunctionCall :: SyntaxPartition -> [SyntaxTree]
treeFunctionCall (TriplePartition x y []) = [funcId -<= funcArgs]
  where
    funcId = serialTree x
    funcArgs = treeConcurrentBracketGroups y

treeAnonFunction :: SyntaxPartition -> [SyntaxTree]
treeAnonFunction (TriplePartition [] y z) = args ++ funcReturn
  where
    funcReturn = treeSingleBracketGroup z
    args = treeConcurrentBracketGroups y

treeFullDeclaration :: SyntaxPartition -> [SyntaxTree]
treeFullDeclaration (TriplePartition x y z) = [(declaration -<= args) -<= funcReturn]
  where
    declaration = serialTree x
    funcReturn = treeSingleBracketGroup z
    args = treeConcurrentBracketGroups y

treeConcurrentBracketGroups :: [SyntaxUnit] -> [SyntaxTree]
treeConcurrentBracketGroups tus = concatMap treeSingleBracketGroup (groupBrackets tus)
  where
    groupBrackets :: [SyntaxUnit] -> [[SyntaxUnit]]
    groupBrackets [] = [[]]
    groupBrackets tus = groupAllTopLevelNestedCollapsibles bracketNestCase tus

treeSingleBracketGroup :: [SyntaxUnit] -> [SyntaxTree]
treeSingleBracketGroup [] = [Empty]
treeSingleBracketGroup xs
  | isCompleteNestedCollapsible bracketNestCase xs = treeSingleBracketGroup (takeNestWhileComplete bracketNestCase xs)
  | hasNestedCollapsible bracketNestCase xs = concatMap syntaxChunkTree $ groupSyntaxChunks xs
  | otherwise = [serialTree xs]


nthChildMeetsCondition :: Int -> (SyntaxTree -> Bool) -> SyntaxTree -> Bool
nthChildMeetsCondition n f st
  | n < 0 = nthChildMeetsCondition ((length . treeChildren) st + n) f st
  | n > ((length . treeChildren) st - 1) = False
  | otherwise = (f . (!! n) . treeChildren) st