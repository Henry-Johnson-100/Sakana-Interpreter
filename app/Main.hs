module Main where

import TreeInterpreter (executeMain, getMainEnvironmentStack, getMainExecutionTrees)
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
  fileTree <- return . generateSyntaxTree . tokenize $ contents
  result <-
    executeMain
      ((return . getMainEnvironmentStack) fileTree)
      ((return . getMainExecutionTrees) fileTree)
  (hPutStrLn stdout . D.fromData) result
  hClose handle