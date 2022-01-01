{-# LANGUAGE MagicHash #-}

module Interpreter.SknStdLib.IO
  ( exporting,
  )
where

import qualified Exception.Base as Exception
import qualified Interpreter.SknStdLib.Type as StdLibType
import qualified Parser.Syntax as Syntax
import System.IO (hGetContents, hPutStr, stderr, stdin, stdout)
import qualified Util.Classes as UC
import Util.Tree (Tree ((:-<-:)))
import qualified Util.Tree as Tree

exporting :: [StdLibType.SknStdLibFunction]
exporting = [trout, herring, dolphin]

trout :: StdLibType.SknStdLibFunction
trout = StdLibType.GeneralStdLibFunction "trout" trout_params# trout#
  where
    trout_params# = ["to_print"]
    trout# trs = trout_definition# trs
      where
        trout_definition#
          (((Syntax.SyntaxUnit (Syntax.Data (Syntax.String str)) _ _) :-<-: []) : []) =
            hPutStr stdout str >> return UC.defaultValue
        trout_definition# (tr : []) = (Exception.raiseError . printTypeException) tr
        trout_definition# trs =
          StdLibType.raiseSknStdLibArgumentException (trs) ("") (trout_params#)

herring :: StdLibType.SknStdLibFunction
herring = StdLibType.GeneralStdLibFunction "herring" herring_params# herring#
  where
    herring_params# = ["to_print"]
    herring# trs = herring_definition# trs
      where
        herring_definition#
          (((Syntax.SyntaxUnit (Syntax.Data (Syntax.String str)) _ _) :-<-: []) : []) =
            hPutStr stderr str >> return UC.defaultValue
        herring_definition# (tr : []) = (Exception.raiseError . printTypeException) tr
        herring_definition# trs =
          StdLibType.raiseSknStdLibArgumentException (trs) ("") (herring_params#)

dolphin :: StdLibType.SknStdLibFunction
dolphin = StdLibType.GeneralStdLibFunction dolphin_id# dolphin_params# dolphin#
  where
    dolphin_id# = "dolphin"
    dolphin_params# = []
    dolphin# trs = dolphin_definition# trs
      where
        dolphin_definition# [] = do
          inString <- hGetContents stdin
          let stringDataTree =
                Tree.tree
                  UC.defaultValue {Syntax.token = Syntax.Data (Syntax.String inString)}
          return stringDataTree
        dolphin_definition# trs =
          StdLibType.raiseSknStdLibArgumentException
            (trs)
            ("This function takes no arguments!")
            (dolphin_params#)

printTypeException :: Syntax.SyntaxTree -> Exception.Exception
printTypeException tr =
  Exception.newException
    Exception.TypeException
    ( ( map Syntax.line
          . Tree.flattenTree
      )
        tr
    )
    ("Error printing tree: " ++ UC.format tr ++ "\n Not a string primitive type.")
    Exception.Fatal