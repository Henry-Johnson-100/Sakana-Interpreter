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
import Token.Util.MonadicContainer
import Token.Util.NestedCollapsible
import Token.Util.Tree

type ParseTree = Tree TokenUnit

type ParseTreeMonad = MonadicContainer ParseTree

type ReturnPartition = TriplePartition TokenUnit

generateParseTree :: [TokenUnit] -> ParseTree
generateParseTree [] = Empty
generateParseTree tus = get $ collapseParseTreeMonadList $ map putReturnPartition $ groupReturnPartitions tus

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
putReturnPartition :: ReturnPartition -> ParseTreeMonad
putReturnPartition (TriplePartition [] [] []) = put (Empty :: ParseTree)
putReturnPartition (TriplePartition x [] []) = putOnlyNonTerminals (TriplePartition x [] [])
putReturnPartition (TriplePartition [] y []) = collapseParseTreeMonadList $ putConcurrentBracketGroups y
putReturnPartition (TriplePartition [] [] z) = putOnlyValue (TriplePartition [] [] z)
putReturnPartition (TriplePartition x [] z) = putNoArgs (TriplePartition x [] z)
putReturnPartition (TriplePartition x y []) = putFunctionCall (TriplePartition x y [])
putReturnPartition (TriplePartition [] y z) = putAnonFunction (TriplePartition [] y z)
putReturnPartition (TriplePartition x y z) = putFullDeclaration (TriplePartition x y z)

putOnlyNonTerminals :: ReturnPartition -> ParseTreeMonad
putOnlyNonTerminals (TriplePartition x [] []) = put (serialTree x)

putOnlyValue :: ReturnPartition -> ParseTreeMonad
putOnlyValue (TriplePartition [] [] z) = putSingleBracketGroup z

putNoArgs :: ReturnPartition -> ParseTreeMonad
putNoArgs (TriplePartition x [] z) = do
  declaration <- put (serialTree x)
  funcReturn <- putSingleBracketGroup z
  putLiftHeadlessChildren $ declaration -<- funcReturn

putFunctionCall :: ReturnPartition -> ParseTreeMonad
putFunctionCall (TriplePartition x y []) = do
  funcId <- put (serialTree x)
  funcArgs <- collapseParseTreeMonadList $ putConcurrentBracketGroups y
  putLiftHeadlessChildren $ funcId -<- funcArgs

putAnonFunction :: ReturnPartition -> ParseTreeMonad
putAnonFunction (TriplePartition [] y z) = do
  funcReturn <- putSingleBracketGroup z
  args <- collapseParseTreeMonadList $ putConcurrentBracketGroups y
  put $ (Headless [] -<= treeChildren args) -<- funcReturn

putFullDeclaration :: ReturnPartition -> ParseTreeMonad
putFullDeclaration (TriplePartition x y z) = do
  declaration <- put (serialTree x)
  funcReturn <- putSingleBracketGroup z
  args <- collapseParseTreeMonadList $ putConcurrentBracketGroups y
  putLiftHeadlessChildren $ (declaration -<= treeChildren args) -<- funcReturn

collapseParseTreeMonadList :: [ParseTreeMonad] -> ParseTreeMonad
collapseParseTreeMonadList [] = put (Empty :: ParseTree)
collapseParseTreeMonadList ptms = put $ Headless (map get ptms)

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

putLiftHeadlessChildren :: ParseTree -> ParseTreeMonad
putLiftHeadlessChildren t = put $ liftHeadlessChildren' t
  where
    liftHeadlessChildren' :: ParseTree -> ParseTree
    liftHeadlessChildren' (Headless cs) = Headless cs
    liftHeadlessChildren' (n :-<-: cs) = tree n -<= concatMap (\c -> if isHeadless c then treeChildren c else [c]) cs