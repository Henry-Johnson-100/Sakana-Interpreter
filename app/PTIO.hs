import System.IO
import System.Environment
import ParseTree
import Lexer
import Token.Data

printTokens xs = concat $ map (show) $ tokenize xs

printTree c = fPrintTree 0 (generateParseTree (tokenize c) (ParseTree (PacketUnit ((Data (Id "Main"))) 0) []))

main = do
    args <- getArgs
    handle <- openFile (head args) ReadMode
    contents <- hGetContents handle
    putStr $ printTree contents
    hClose handle