import System.IO
import System.Environment
import ParseTree
import Lexer (tokenize)

printTokens xs = concat $ map (show) $ tokenize xs

printTree c = fPrintTree 0 (generateParseTreeFromTopLevelBlock $ tokenize c)

main = do
    args <- getArgs
    handle <- openFile (head args) ReadMode
    contents <- hGetContents handle
    putStr $ printTree contents
    hClose handle