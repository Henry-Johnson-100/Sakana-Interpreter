module SyntaxTree
  ( generateSyntaxTree,
    generateModuleTree,
    SyntaxTree,
    TreeIO (..),
    SyntaxUnit (..),
  )
where

import Data.List (mapAccumL)
import Lexer
import Token.Bracket
  ( BracketTerminal (Close, Open),
    ScopeType (Return, Send),
  )
import Token.Data (Data (Id))
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

generateModuleTree :: String -> [TokenUnit] -> SyntaxTree
generateModuleTree name = flip nameModuleTree name . generateSyntaxTree

nameModuleTree :: SyntaxTree -> String -> SyntaxTree
nameModuleTree (_ :-<-: cs) str = tree (SyntaxUnit (Data (Id str)) 0 Return) -<= cs

generateSyntaxTree :: [TokenUnit] -> SyntaxTree
generateSyntaxTree [] = Empty
generateSyntaxTree tus = tree (SyntaxUnit (Data (Id "main")) 0 Return) -<= concatMap syntaxChunkTree ((groupSyntaxChunks . scanTokensToSyntaxes) tus)

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
