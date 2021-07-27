module Main where

import Token.Util.EagerCollapsible

main :: IO ()
main = putStrLn $ concat $ dropInfix ["2","3"] ["1","2","3","4"]
