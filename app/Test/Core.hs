module Test.Core
  ( module Test.Tasty,
    module Test.Tasty.HUnit,
    standardTimeout,
    timedTest,
    timedAssertEqual,
    dataId,
    dataNum,
    dataString,
    makeSU,
    treeSU,
    treeId,
    listIds,
    attachToMain,
    attachToLamprey,
    attachToShoal,
  )
where

import Parser.Core (generalParse)
import Syntax
  ( Data (Id, Num, String),
    Keyword (Lamprey, Shoal),
    ScopeType (Send),
    SyntaxTree,
    SyntaxUnit (SyntaxUnit, token),
    Token (Data, Keyword),
  )
import Test.Tasty
import Test.Tasty.HUnit
import Util.Classes (Emptiable (empty))
import Util.General ((.<))
import Util.Tree (Tree, tree, (-<=))

standardTimeout :: Integer -> TestTree -> TestTree
standardTimeout timeS = localOption (Timeout (timeS * 1000000) (show timeS ++ "s"))

timedTest :: Integer -> TestName -> Assertion -> TestTree
timedTest timeS name = (standardTimeout timeS . testCase name)

timedAssertEqual ::
  (Eq p, Show p) =>
  Integer ->
  TestName ->
  [Char] ->
  p ->
  p ->
  TestTree
timedAssertEqual timeS name description_optional assert func =
  timedTest timeS name assertion
  where
    assertion = assertEqual d a f
    d = if null description_optional then name else description_optional
    a = assert
    f = func

dataId :: String -> Token
dataId = Data . Id

dataNum :: Double -> Token
dataNum = Data . Num

dataString :: String -> Token
dataString = Data . String

makeSU :: ScopeType -> Token -> SyntaxUnit
makeSU c t = SyntaxUnit t 1 c

treeSU :: ScopeType -> Token -> Tree SyntaxUnit
treeSU = tree .< makeSU

treeId :: ScopeType -> String -> SyntaxTree
treeId st = treeSU st . dataId

listIds :: ScopeType -> [[Char]] -> [SyntaxTree]
listIds st = (<$>) (treeSU st) . (<$>) dataId

sendArgs :: [[Char]] -> [SyntaxTree]
sendArgs = listIds Send

attachToMain :: [SyntaxTree] -> SyntaxTree
attachToMain = (-<=) (tree (empty) {token = Data (Id "main")})

attachToLamprey :: [Tree SyntaxUnit] -> Tree SyntaxUnit
attachToLamprey = (-<=) (tree (empty {token = Keyword Lamprey}))

attachToShoal :: [Tree SyntaxUnit] -> Tree SyntaxUnit
attachToShoal = (-<=) (tree (empty {token = Keyword Shoal}))