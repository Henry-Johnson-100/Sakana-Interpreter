module ExecutionTree (

) where

import SyntaxTree
import Lexer
import Token.Util.Tree
import Token.Keyword
import Token.Bracket
import Token.Data
import Token.Operator

s' = "fish add >(n)> >(m)> <(+ >(n)> >(m)> )< fish sub >(n)> >(m)> <(- >(n)> >(m)> )< <(add >(1)> >(1)>)<"

t' = tokenize s'

pt' = generateSyntaxTree t'

{-The most fundamental node execution is returning a primitive value
After that, performing a primitive operation like addition or subtraction
After that, calling a built-in function,
After that, defining a function,
After that, calling a defined function.-}
