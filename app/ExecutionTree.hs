module ExecutionTree (

) where

import ParseTree
import Lexer
import Token.Util.Tree
import Token.Keyword
import Token.Data
import Token.Operator

type ReferenceTree = Tree TokenUnit --A semantic difference, used to denote a tree used for lookups

s' = "fish add >(n)> >(m)> <(+ >(n)> >(m)> )< fish sub >(n)> >(m)> <(- >(n)> >(m)> )<"

t' = tokenize s'

pt' = generateParseTree t'

lookupAllFunctionDeclarations :: ParseTree -> [ReferenceTree]
lookupAllFunctionDeclarations pt = lookupOn pt (\t -> not (isHeadless t) && unit (treeNode t) == Keyword Fish)

lookupSummarySingleValue :: [ReferenceTree] -> ReferenceTree
lookupSummarySingleValue [] = Empty
lookupSummarySingleValue rts = head rts

getFunctionDeclId :: ReferenceTree -> String
getFunctionDeclId Empty = ""
getFunctionDeclId rt = fromToken $ unit $ treeNode $ head $ treeChildren rt

lookupFunction :: ParseTree -> String -> ReferenceTree
lookupFunction Empty _ = Empty
lookupFunction pt id = lookupSummarySingleValue $ filter (\x -> id == getFunctionDeclId x) (lookupAllFunctionDeclarations pt)

executeNode :: ReferenceTree -> String --experimenting with simple operators right now
executeNode ((PacketUnit (Operator Add) _) :-<-: [PacketUnit (Data (Int x)) _ :-<-: [],PacketUnit (Data (Int y)) _ :-<-: []]) = show (x + y)

execute :: String -> String
execute fish = executeNode $ head $ treeChildren $ generateParseTree $ tokenize fish