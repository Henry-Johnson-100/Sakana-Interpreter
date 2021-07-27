module AbstractSyntaxTree (

) where

import Data.List
import Lexer          as L
import Token.Keyword  as K
import Token.Bracket  as B
import Token.Control  as C
import Token.Data     as D
import Token.Operator as O


data AST a = None | Node Token (AST a) (AST a) deriving (Eq, Show, Read)
type Coral = AST

testTokenList = [Keyword Fish, L.Data (D.Data "factorial"), Bracket (Send Open), L.Data (D.Data "n"),Bracket (Send Close), Bracket (Return Open),L.Control Fin, Bracket (Send Open),L.Data (D.Data "n"), Operator Eq ,L.Data (D.Data "0"),L.Data (D.Data "1"), Bracket (Send Close),L.Data (D.Data "n"), Operator Mult, L.Data (D.Data "factorial"), Bracket (Send Open), Bracket (Return Open), L.Data (D.Data "n"), Operator Sub, L.Data (D.Data "1"), Bracket (Return Close), Bracket (Send Close), Bracket (Return Close)]

{-
route factorial >(n)> <(
    fin >( n == 0 , 1 )>
    n * factorial >(<(n - 1)<)>
)<
-}

insertToken :: Token -> Coral Token -> Scope -> Coral Token
insertToken t None _ = (Node t None None)
insertToken t (Node pToken send return) Open = (Node pToken (insertToken t send Open) return)
insertToken t (Node pToken send return) Close = (Node pToken send (insertToken t return Close))

insertOpen :: [Token] -> Coral Token -> Coral Token
insertOpen ts node = foldl' (\n t -> insertToken t n Open) node ts

insertClose :: [Token] -> Coral Token -> Coral Token
insertClose ts node = foldl' (\n t -> insertToken t n Close) node ts

filterNotBrackets :: [Token] -> [Token]
filterNotBrackets ts = filterNotLike (Bracket (Send Open)) ts

partitionWhile :: (a -> Bool) -> [a] -> ([a],[a])
partitionWhile f xs = (takeWhile f xs, dropWhile f xs)

partitionBracketSet :: [Token] -> ([Token],[Token])
partitionBracketSet ts = partitionWhile (\x -> not (x `L.like` (Bracket (Send Open)))) ts

insertTokenList :: [Token] -> Coral Token -> Coral Token
--insertTokenList (t:ts) None = insertTokenList (ts) (Node t None None)
insertTokenList (t:ts) node
    | null ts = node
    | t == (Bracket (Send Open))   = insertTokenList (snd bracketTuple) (insertOpen (fst bracketTuple) node)
    | t == (Bracket (Return Open)) = insertTokenList (snd bracketTuple) (insertClose (fst bracketTuple) node)
    | otherwise                    = insertTokenList ts node
    where
        bracketTuple = partitionBracketSet ts