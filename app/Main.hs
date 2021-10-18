module Main where

import ExecutionTree (executeMain)
import SyntaxTree ( generateSyntaxTree )
import Lexer ( tokenize )
import System.Environment (getArgs)
import System.IO
    ( hClose, hGetContents, openFile, IOMode(ReadMode) )

main :: IO ()
main = do
    args <- getArgs
    handle <- openFile (head args) ReadMode
    contents <-hGetContents handle
    (print . executeMain . generateSyntaxTree . tokenize) contents
    hClose handle