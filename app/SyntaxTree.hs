{-# LANGUAGE MagicHash #-}

module SyntaxTree
  ( SyntaxTree,
    TreeStruct.TreeIO,
    SyntaxUnit (..),
    generateSyntaxTree,
    generateModuleTree,
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
import qualified Token.Util.EagerCollapsible as EagerCollapsible (dropInfix)
import qualified Token.Util.NestedCollapsible as NestedCollapsible
  ( NCCase (NCCase),
    TriplePartition (..),
    breakByNest,
    groupByPartition,
    hasNestedCollapsible,
    isCompleteNestedCollapsible,
    nestedCollapsibleIsPrefixOf,
    takeNestWhileComplete,
    takeWhileList,
  )
import qualified Token.Util.Tree as TreeStruct
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
    (-<=),
  )

data SyntaxUnit = SyntaxUnit
  { token :: Lexer.Token,
    line :: Int,
    context :: B.ScopeType
  }
  deriving (Show, Eq)

genericSyntaxUnit :: Lexer.Token -> SyntaxUnit
genericSyntaxUnit t = SyntaxUnit t 0 B.Return

setContext :: SyntaxUnit -> B.ScopeType -> SyntaxUnit
setContext su st = su {context = st}

type SyntaxTree = TreeStruct.Tree SyntaxUnit

type SyntaxPartition = NestedCollapsible.TriplePartition SyntaxUnit

generateModuleTree :: String -> [Lexer.TokenUnit] -> SyntaxTree
generateModuleTree name = flip nameModuleTree name . generateSyntaxTree

nameModuleTree :: SyntaxTree -> String -> SyntaxTree
nameModuleTree tr str =
  TreeStruct.mutateTreeNode
    tr
    (\_ -> genericSyntaxUnit (Lexer.Data (D.Id str)))

generateSyntaxTree :: [Lexer.TokenUnit] -> SyntaxTree
generateSyntaxTree [] = TreeStruct.Empty
generateSyntaxTree tus =
  (declarationErrorCheck# . reContextualizeSchoolMethods) $
    TreeStruct.tree (genericSyntaxUnit (Lexer.Data (D.Id "main")))
      TreeStruct.-<= concatMap
        syntaxPartitionTree
        ((getSyntaxPartitions . scanTokensToSyntaxes) tus)

bracketNestCase :: NestedCollapsible.NCCase SyntaxUnit
bracketNestCase =
  NestedCollapsible.NCCase
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
    tokenUnitToSyntaxUnit :: Lexer.TokenUnit -> B.ScopeType -> SyntaxUnit
    tokenUnitToSyntaxUnit tu = SyntaxUnit (Lexer.unit tu) (Lexer.unitLine tu)
    scanScopeTypes :: [Lexer.TokenUnit] -> [B.ScopeType]
    scanScopeTypes [] = []
    scanScopeTypes tus = scanl getScanScopeType B.Return tus
      where
        getScanScopeType :: B.ScopeType -> Lexer.TokenUnit -> B.ScopeType
        getScanScopeType _ (Lexer.PacketUnit (Lexer.Bracket B.Send B.Open) _) = B.Send
        getScanScopeType _ (Lexer.PacketUnit (Lexer.Bracket B.Return B.Open) _) = B.Return
        getScanScopeType st _ = st

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
  [TreeStruct.serialTree x]

treeOnlyValue :: SyntaxPartition -> [SyntaxTree]
treeOnlyValue (NestedCollapsible.TriplePartition [] [] z) = treeSingleBracketGroup z

treeNoArgs :: SyntaxPartition -> [SyntaxTree]
treeNoArgs (NestedCollapsible.TriplePartition x [] z) =
  [declaration TreeStruct.-<= funcReturn]
  where
    declaration = TreeStruct.serialTree x
    funcReturn = treeSingleBracketGroup z

treeFunctionCall :: SyntaxPartition -> [SyntaxTree]
treeFunctionCall (NestedCollapsible.TriplePartition x y []) =
  [funcId TreeStruct.-<= funcArgs]
  where
    funcId = TreeStruct.serialTree x
    funcArgs = treeConcurrentBracketGroups y

treeAnonFunction :: SyntaxPartition -> [SyntaxTree]
treeAnonFunction (NestedCollapsible.TriplePartition [] y z) = args ++ funcReturn
  where
    funcReturn = treeSingleBracketGroup z
    args = treeConcurrentBracketGroups y

treeFullDeclaration :: SyntaxPartition -> [SyntaxTree]
treeFullDeclaration (NestedCollapsible.TriplePartition x y z) =
  [(declaration TreeStruct.-<= args) TreeStruct.-<= funcReturn]
  where
    declaration = TreeStruct.serialTree x
    funcReturn = treeSingleBracketGroup z
    args = treeConcurrentBracketGroups y

treeConcurrentBracketGroups :: [SyntaxUnit] -> [SyntaxTree]
treeConcurrentBracketGroups tus = concatMap treeSingleBracketGroup (groupBrackets tus)
  where
    groupBrackets :: [SyntaxUnit] -> [[SyntaxUnit]]
    groupBrackets [] = [[]]
    groupBrackets tus = groupTopLevelAndErrorCheck --this is where free tokens get removed
      where
        groupTopLevelAndErrorCheck =
          map
            (NestedCollapsible.partSnd . partitionHasFreeTokenErrorCheck#)
            (NestedCollapsible.groupByPartition bracketNestCase tus)
        partitionHasFreeTokenErrorCheck# :: SyntaxPartition -> SyntaxPartition
        partitionHasFreeTokenErrorCheck# sp
          | (not . null . NestedCollapsible.partFst) sp =
            freeTokenError# (NestedCollapsible.partFst sp)
          | otherwise = sp
          where
            -- Never returns anything, therefore,
            -- this function signature is useless at best or misleading at worst
            freeTokenError# :: [SyntaxUnit] -> SyntaxPartition
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

treeSingleBracketGroup :: [SyntaxUnit] -> [SyntaxTree]
treeSingleBracketGroup [] = [(TreeStruct.tree . genericSyntaxUnit) (Lexer.Data D.Null)]
treeSingleBracketGroup xs
  | NestedCollapsible.isCompleteNestedCollapsible bracketNestCase xs =
    treeSingleBracketGroup (NestedCollapsible.takeNestWhileComplete bracketNestCase xs)
  | NestedCollapsible.hasNestedCollapsible bracketNestCase xs =
    concatMap syntaxPartitionTree $ getSyntaxPartitions xs
  | otherwise = [TreeStruct.serialTree xs]

reContextualizeSchoolMethods :: SyntaxTree -> SyntaxTree
reContextualizeSchoolMethods TreeStruct.Empty = TreeStruct.Empty
reContextualizeSchoolMethods st
  | null
      ( TreeStruct.lookupOn
          st
          ( \x ->
              (token . Data.Maybe.fromJust . TreeStruct.treeNode) x
                == Lexer.Keyword K.School
          )
      ) =
    st
  | (token . Data.Maybe.fromJust . TreeStruct.treeNode) st /= Lexer.Keyword K.School =
    TreeStruct.reTree st
      TreeStruct.-<= TreeStruct.childMap
        reContextualizeSchoolMethods
        st
  | otherwise =
    TreeStruct.reTree st
      TreeStruct.-<= map
        reContextualizeSchoolMethods
        reContextualizedChildren
  where
    reContextualizedChildren :: [SyntaxTree]
    reContextualizedChildren =
      fst breakOnSendReturn
        ++ map
          (\x -> TreeStruct.mutateTreeNode x (`setContext` B.Return))
          (snd breakOnSendReturn)
    breakOnSendReturn =
      span
        (\x -> B.Send == (context . Data.Maybe.fromJust . TreeStruct.treeNode) x)
        (TreeStruct.treeChildren st)

declarationErrorCheck# :: SyntaxTree -> SyntaxTree
declarationErrorCheck# =
  readTreeForError#
    ( TreeStruct.maybeOnTreeNode
        False
        (Lexer.keywordTokenIsDeclarationRequiringId . token)
    )
    (declarationHasIdToken# . declarationIdHasNoChildren#)
  where
    declarationHasIdToken# tr
      | nthChildMeetsCondition
          0
          (TreeStruct.maybeOnTreeNode False (Lexer.dataTokenIsId . token))
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
          (not . any (TreeStruct.Empty /=) . TreeStruct.treeChildren)
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
          concat . TreeStruct.childrenOfChildren . head . TreeStruct.treeChildren

nthChildMeetsCondition :: Int -> (SyntaxTree -> Bool) -> SyntaxTree -> Bool
nthChildMeetsCondition n f st
  | n < 0 = nthChildMeetsCondition ((length . TreeStruct.treeChildren) st + n) f st
  | n > ((length . TreeStruct.treeChildren) st - 1) = False
  | otherwise = (f . (!! n) . TreeStruct.treeChildren) st

getSyntaxAttributeFromTree :: (SyntaxUnit -> a) -> SyntaxTree -> a
getSyntaxAttributeFromTree attr =
  TreeStruct.maybeOnTreeNode ((attr . genericSyntaxUnit) (Lexer.Data D.Null)) attr

readTreeForError# ::
  (SyntaxTree -> Bool) ->
  (SyntaxTree -> SyntaxTree) ->
  SyntaxTree ->
  SyntaxTree
readTreeForError# _ _ TreeStruct.Empty = TreeStruct.Empty
readTreeForError# stopOn readF tr
  | stopOn tr =
    (TreeStruct.reTree . readF) tr
      TreeStruct.-<= map
        (readTreeForError# stopOn readF)
        (TreeStruct.treeChildren tr)
  | otherwise =
    TreeStruct.reTree tr
      TreeStruct.-<= map
        (readTreeForError# stopOn readF)
        (TreeStruct.treeChildren tr)
