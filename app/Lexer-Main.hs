import System.IO
import Lexer

main = do
    handle <- openFile "G:/FishShit/FISH/Test/Lexer-MainTest.fish" ReadMode
    contents <- hGetContents handle
    putStr contents
    putStr $ concat $ map (show) $ tokenize contents
    hClose handle