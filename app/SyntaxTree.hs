{-# LANGUAGE MagicHash #-}

-- #TODO institute error checking for nodes containing primitive data types
-- They should not have children trees, if so, they will probably be ignored

-- binary operators should only have two children
module SyntaxTree
  ( SyntaxTree,
    Tree.TreeIO,
    SyntaxUnit (..),
    generateSyntaxTree,
    generateModuleTree,
    genericSyntaxUnit,
    getSyntaxAttributeFromTree,
    nthChildMeetsCondition,
    setContext,
    tokenUnitToSyntaxUnit,
  )
where

import qualified Data.List (intercalate)
import qualified Data.Maybe (fromJust)
import qualified Exception.Base
  ( ExceptionSeverity (Fatal, NonFatal),
    ExceptionType (DeclarationMissingId, FreeTokensInForeignScope),
    newException,
    raiseError,
  )
import qualified Lexer
  ( PacketUnit (PacketUnit, unit, unitLine),
    Token (Bracket, Data, Keyword),
    TokenUnit,
    dataTokenIsId,
    fromToken,
    getTokenBracketScopeType,
    keywordTokenIsDeclarationRequiringId,
  )
import qualified Token.Bracket as B
  ( BracketTerminal (Close, Open),
    ScopeType (..),
  )
import qualified Token.Data as D (Data (Id, Null))
import qualified Token.Keyword as K (Keyword (School))
import qualified Util.CollapsibleTerminalCases as CTC (CollapsibleTerminalCases (..))
import qualified Util.EagerCollapsible as EagerCollapsible (dropInfix)
import qualified Util.NestedCollapsible as NestedCollapsible
  ( TriplePartition (..),
    breakByNest,
    groupByPartition,
    hasNestedCollapsible,
    isCompleteNestedCollapsible,
    nestedCollapsibleIsPrefixOf,
    takeNestWhileComplete,
    takeWhileList,
  )
import Util.Tree (Tree ((:-<-:)), (-<=))
import qualified Util.Tree as Tree
  ( Tree (Empty),
    TreeIO (..),
    childMap,
    childrenOfChildren,
    lookupOn,
    maybeOnTreeNode,
    mutateTreeNode,
    reTree,
    serialTree,
    tree,
    treeChildren,
    treeNode,
  )

data SyntaxUnit = SyntaxUnit
  { token :: Lexer.Token,
    line :: Int,
    context :: B.ScopeType
  }
  deriving (Show, Eq)

genericSyntaxUnit :: Lexer.Token -> SyntaxUnit
genericSyntaxUnit t = SyntaxUnit t 0 B.Return

setContext :: B.ScopeType -> SyntaxUnit -> SyntaxUnit
setContext st su = su {context = st}

type SyntaxTree = Tree.Tree SyntaxUnit

type SyntaxPartition = NestedCollapsible.TriplePartition SyntaxUnit

generateModuleTree :: String -> [Lexer.TokenUnit] -> SyntaxTree
generateModuleTree name = flip nameModuleTree name . generateSyntaxTree

nameModuleTree :: SyntaxTree -> String -> SyntaxTree
nameModuleTree tr str =
  Tree.mutateTreeNode
    tr
    (\_ -> genericSyntaxUnit (Lexer.Data (D.Id str)))

generateSyntaxTree :: [Lexer.TokenUnit] -> SyntaxTree
generateSyntaxTree [] = Tree.Empty
generateSyntaxTree tus =
  (declarationErrorCheck# . reContextualizeSchoolMethods) $
    genericSyntaxUnit (Lexer.Data (D.Id "main"))
      :-<-: concatMap
        syntaxPartitionTree
        ((getSyntaxPartitions . scanTokensToSyntaxes) tus)

bracketNestCase :: CTC.CollapsibleTerminalCases SyntaxUnit
bracketNestCase =
  CTC.CollapsibleTerminalCases
    (\x -> token x `elem` [Lexer.Bracket B.Send B.Open, Lexer.Bracket B.Return B.Open])
    (\x -> token x `elem` [Lexer.Bracket B.Send B.Close, Lexer.Bracket B.Return B.Close])

takeBracketNestedCollapsibleExcludingReturn :: [SyntaxUnit] -> [SyntaxUnit]
takeBracketNestedCollapsibleExcludingReturn [] = []
takeBracketNestedCollapsibleExcludingReturn (tu : tus)
  | null (NestedCollapsible.partSnd part) = []
  | (Lexer.getTokenBracketScopeType . token . head . NestedCollapsible.partSnd) part
      == B.Return =
    []
  | otherwise =
    partFstSnd
      ++ takeBracketNestedCollapsibleExcludingReturn (NestedCollapsible.partThd part)
  where
    part = NestedCollapsible.breakByNest bracketNestCase (tu : tus)
    partFstSnd = NestedCollapsible.partFst part ++ NestedCollapsible.partSnd part

takeBracketNestedCollapsibleIncludingReturn :: [SyntaxUnit] -> [SyntaxUnit]
takeBracketNestedCollapsibleIncludingReturn [] = []
takeBracketNestedCollapsibleIncludingReturn (tu : tus)
  | null (NestedCollapsible.partSnd part) = []
  | Lexer.getTokenBracketScopeType (token (head (NestedCollapsible.partSnd part)))
      == B.Return =
    partFstSnd
  | otherwise =
    partFstSnd
      ++ takeBracketNestedCollapsibleIncludingReturn (NestedCollapsible.partThd part)
  where
    part = NestedCollapsible.breakByNest bracketNestCase (tu : tus)
    partFstSnd = NestedCollapsible.partFst part ++ NestedCollapsible.partSnd part

scanTokensToSyntaxes :: [Lexer.TokenUnit] -> [SyntaxUnit]
scanTokensToSyntaxes [] = []
scanTokensToSyntaxes tus = zipWith tokenUnitToSyntaxUnit tus (scanScopeTypes tus)
  where
    scanScopeTypes :: [Lexer.TokenUnit] -> [B.ScopeType]
    scanScopeTypes [] = []
    scanScopeTypes tus = scanl getScanScopeType B.Return tus
      where
        getScanScopeType :: B.ScopeType -> Lexer.TokenUnit -> B.ScopeType
        getScanScopeType _ (Lexer.PacketUnit (Lexer.Bracket B.Send B.Open) _) = B.Send
        getScanScopeType _ (Lexer.PacketUnit (Lexer.Bracket B.Return B.Open) _) = B.Return
        getScanScopeType st _ = st

tokenUnitToSyntaxUnit :: Lexer.TokenUnit -> B.ScopeType -> SyntaxUnit
tokenUnitToSyntaxUnit tu = SyntaxUnit (Lexer.unit tu) (Lexer.unitLine tu)

getSyntaxPartitions :: [SyntaxUnit] -> [SyntaxPartition]
getSyntaxPartitions [] = []
getSyntaxPartitions tus = map syntaxPartitionFromChunk (groupSyntaxChunks tus)
  where
    syntaxPartitionFromChunk :: [SyntaxUnit] -> SyntaxPartition
    syntaxPartitionFromChunk [] = NestedCollapsible.TriplePartition [] [] []
    syntaxPartitionFromChunk tus = NestedCollapsible.TriplePartition w a r
      where
        w =
          NestedCollapsible.takeWhileList
            (not . NestedCollapsible.nestedCollapsibleIsPrefixOf bracketNestCase)
            tus
        a = takeBracketNestedCollapsibleExcludingReturn (EagerCollapsible.dropInfix w tus)
        r = EagerCollapsible.dropInfix (w ++ a) tus
    groupSyntaxChunks :: [SyntaxUnit] -> [[SyntaxUnit]]
    groupSyntaxChunks [] = []
    groupSyntaxChunks tus' =
      (fst . spanOnSyntaxChunk) tus' : (groupSyntaxChunks . snd . spanOnSyntaxChunk) tus'
      where
        spanOnSyntaxChunk :: [SyntaxUnit] -> ([SyntaxUnit], [SyntaxUnit])
        spanOnSyntaxChunk [] = ([], [])
        spanOnSyntaxChunk tus =
          (takenThroughReturn, EagerCollapsible.dropInfix takenThroughReturn tus)
          where
            takenThroughReturn = takeBracketNestedCollapsibleIncludingReturn tus

-- | Receptacle for all possible pattern matches of a TriplePartition when making a tree
syntaxPartitionTree :: SyntaxPartition -> [SyntaxTree]
syntaxPartitionTree (NestedCollapsible.TriplePartition [] [] []) =
  []
syntaxPartitionTree (NestedCollapsible.TriplePartition x [] []) =
  treeOnlyNonTerminals (NestedCollapsible.TriplePartition x [] [])
syntaxPartitionTree (NestedCollapsible.TriplePartition [] y []) =
  treeConcurrentBracketGroups y
syntaxPartitionTree (NestedCollapsible.TriplePartition [] [] z) =
  treeOnlyValue (NestedCollapsible.TriplePartition [] [] z)
syntaxPartitionTree (NestedCollapsible.TriplePartition x [] z) =
  treeNoArgs (NestedCollapsible.TriplePartition x [] z)
syntaxPartitionTree (NestedCollapsible.TriplePartition x y []) =
  treeFunctionCall (NestedCollapsible.TriplePartition x y [])
syntaxPartitionTree (NestedCollapsible.TriplePartition [] y z) =
  treeAnonFunction (NestedCollapsible.TriplePartition [] y z)
syntaxPartitionTree (NestedCollapsible.TriplePartition x y z) =
  treeFullDeclaration (NestedCollapsible.TriplePartition x y z)

treeOnlyNonTerminals :: SyntaxPartition -> [SyntaxTree]
treeOnlyNonTerminals (NestedCollapsible.TriplePartition x [] []) =
  [Tree.serialTree x]

treeOnlyValue :: SyntaxPartition -> [SyntaxTree]
treeOnlyValue (NestedCollapsible.TriplePartition [] [] z) =
  treeSingleBracketGroup B.Return z

treeNoArgs :: SyntaxPartition -> [SyntaxTree]
treeNoArgs (NestedCollapsible.TriplePartition x [] z) =
  [declaration -<= funcReturn]
  where
    declaration = Tree.serialTree x
    funcReturn = treeSingleBracketGroup B.Return z

treeFunctionCall :: SyntaxPartition -> [SyntaxTree]
treeFunctionCall (NestedCollapsible.TriplePartition x y []) =
  [funcId -<= funcArgs]
  where
    funcId = Tree.serialTree x
    funcArgs = treeConcurrentBracketGroups y

treeAnonFunction :: SyntaxPartition -> [SyntaxTree]
treeAnonFunction (NestedCollapsible.TriplePartition [] y z) = args ++ funcReturn
  where
    funcReturn = treeSingleBracketGroup B.Return z
    args = treeConcurrentBracketGroups y

treeFullDeclaration :: SyntaxPartition -> [SyntaxTree]
treeFullDeclaration (NestedCollapsible.TriplePartition x y z) =
  [(declaration -<= args) -<= funcReturn]
  where
    declaration = Tree.serialTree x
    funcReturn = treeSingleBracketGroup B.Return z
    args = treeConcurrentBracketGroups y

-- | Only ever called for arguments so it's null values will have a context of send,
--  I, of course, anticipate bugs happening because of this decision.
treeConcurrentBracketGroups :: [SyntaxUnit] -> [SyntaxTree]
treeConcurrentBracketGroups tus =
  concatMap (treeSingleBracketGroup B.Send) (groupBrackets tus)
  where
    groupBrackets :: [SyntaxUnit] -> [[SyntaxUnit]]
    groupBrackets [] = [[]]
    groupBrackets tus = groupTopLevelAndErrorCheck
      where
        groupTopLevelAndErrorCheck =
          map
            (NestedCollapsible.partSnd . partitionHasFreeTokenErrorCheck#)
            (NestedCollapsible.groupByPartition bracketNestCase tus)
        partitionHasFreeTokenErrorCheck# sp
          | (not . null . NestedCollapsible.partFst) sp =
            freeTokenError# (NestedCollapsible.partFst sp)
          | otherwise = sp
          where
            freeTokenError# sus
              | (Lexer.keywordTokenIsDeclarationRequiringId . token . head) sus =
                raiseFreeTokenError#
                  getTokenLines
                  ( "Free tokens in ambiguous scope, \'"
                      ++ getTokenStrings
                      ++ "\' Is there a missing return fish before this declaration?"
                  )
                  Exception.Base.Fatal
              | otherwise =
                raiseFreeTokenError#
                  getTokenLines
                  ( "Free token(s) in ambiguous scope, \'"
                      ++ getTokenStrings
                      ++ "\' are they meant to be in a fish?"
                  )
                  Exception.Base.NonFatal
              where
                getTokenLines = map line sus
                getTokenStrings =
                  (Data.List.intercalate ", " . map (Lexer.fromToken . token)) sus
                raiseFreeTokenError# ln str sev =
                  Exception.Base.raiseError $
                    Exception.Base.newException
                      Exception.Base.FreeTokensInForeignScope
                      ln
                      str
                      sev

treeSingleBracketGroup :: B.ScopeType -> [SyntaxUnit] -> [SyntaxTree]
treeSingleBracketGroup st [] =
  [(Tree.tree . setContext st . genericSyntaxUnit) (Lexer.Data D.Null)]
treeSingleBracketGroup st xs
  | NestedCollapsible.isCompleteNestedCollapsible bracketNestCase xs =
    treeSingleBracketGroup st (NestedCollapsible.takeNestWhileComplete bracketNestCase xs)
  | NestedCollapsible.hasNestedCollapsible bracketNestCase xs =
    concatMap syntaxPartitionTree $ getSyntaxPartitions xs
  | otherwise = [Tree.serialTree xs]

reContextualizeSchoolMethods :: SyntaxTree -> SyntaxTree
reContextualizeSchoolMethods Tree.Empty = Tree.Empty
reContextualizeSchoolMethods st
  | null
      ( Tree.lookupOn
          st
          ( \x ->
              (token . Data.Maybe.fromJust . Tree.treeNode) x
                == Lexer.Keyword K.School
          )
      ) =
    st
  | (token . Data.Maybe.fromJust . Tree.treeNode) st /= Lexer.Keyword K.School =
    Tree.reTree st
      -<= Tree.childMap
        reContextualizeSchoolMethods
        st
  | otherwise =
    Tree.reTree st
      -<= map
        reContextualizeSchoolMethods
        reContextualizedChildren
  where
    reContextualizedChildren =
      fst breakOnSendReturn
        ++ map
          (\x -> Tree.mutateTreeNode x (setContext B.Return))
          (snd breakOnSendReturn)
    breakOnSendReturn =
      span
        (\x -> B.Send == (context . Data.Maybe.fromJust . Tree.treeNode) x)
        (Tree.treeChildren st)

declarationErrorCheck# :: SyntaxTree -> SyntaxTree
declarationErrorCheck# =
  readTreeForError#
    ( Tree.maybeOnTreeNode
        False
        (Lexer.keywordTokenIsDeclarationRequiringId . token)
    )
    (declarationHasIdToken# . declarationIdHasNoChildren#)
  where
    declarationHasIdToken# tr
      | nthChildMeetsCondition
          0
          (Tree.maybeOnTreeNode False (Lexer.dataTokenIsId . token))
          tr =
        tr
      | otherwise =
        Exception.Base.raiseError $
          Exception.Base.newException
            Exception.Base.DeclarationMissingId
            [getSyntaxAttributeFromTree line tr]
            "Declaration missing identification."
            Exception.Base.Fatal
    declarationIdHasNoChildren# tr
      | nthChildMeetsCondition
          0
          (not . any (Tree.Empty /=) . Tree.treeChildren)
          tr =
        tr
      | otherwise =
        Exception.Base.raiseError $
          Exception.Base.newException
            Exception.Base.FreeTokensInForeignScope
            (map (getSyntaxAttributeFromTree line) (allChildrenOfDeclarationId tr))
            ( "Free tokens, \'"
                ++ Data.List.intercalate
                  ", "
                  ( map
                      (Lexer.fromToken . getSyntaxAttributeFromTree token)
                      (allChildrenOfDeclarationId tr)
                  )
                ++ "\' after a declaration Id should not be included"
            )
            Exception.Base.NonFatal
      where
        allChildrenOfDeclarationId =
          concat . Tree.childrenOfChildren . head . Tree.treeChildren

nthChildMeetsCondition :: Int -> (SyntaxTree -> Bool) -> SyntaxTree -> Bool
nthChildMeetsCondition n f st
  | n < 0 = nthChildMeetsCondition ((length . Tree.treeChildren) st + n) f st
  | n > ((length . Tree.treeChildren) st - 1) = False
  | otherwise = (f . (!! n) . Tree.treeChildren) st

getSyntaxAttributeFromTree :: (SyntaxUnit -> a) -> SyntaxTree -> a
getSyntaxAttributeFromTree attr =
  Tree.maybeOnTreeNode ((attr . genericSyntaxUnit) (Lexer.Data D.Null)) attr

readTreeForError# ::
  (SyntaxTree -> Bool) ->
  (SyntaxTree -> SyntaxTree) ->
  SyntaxTree ->
  SyntaxTree
readTreeForError# _ _ Tree.Empty = Tree.Empty
readTreeForError# stopOn readF tr
  | stopOn tr =
    (Tree.reTree . readF) tr
      -<= map
        (readTreeForError# stopOn readF)
        (Tree.treeChildren tr)
  | otherwise =
    Tree.reTree tr
      -<= map
        (readTreeForError# stopOn readF)
        (Tree.treeChildren tr)
