module Main where

import ExecutionTree (executeMain, calct')
import SyntaxTree
import Lexer
import System.Environment (getArgs)
import System.IO

main :: IO ()
main = do
    args <- getArgs
    handle <- openFile (head args) ReadMode
    contents <-hGetContents handle
    (print . executeMain . generateSyntaxTree . tokenize) contents
    hClose handle