module Main where

import ExecutionTree (executeMain)
import Lexer (tokenize)
import SyntaxTree (generateSyntaxTree)
import System.Environment (getArgs)
import System.IO
  ( IOMode (ReadMode),
    hClose,
    hGetContents,
    hPutStrLn,
    openFile,
    stdout,
  )
import qualified Token.Data as D (fromData)

main :: IO ()
main = do
  args <- getArgs
  handle <- openFile (head args) ReadMode
  contents <- hGetContents handle
  (hPutStrLn stdout . D.fromData . executeMain . generateSyntaxTree . tokenize) contents
  hClose handle