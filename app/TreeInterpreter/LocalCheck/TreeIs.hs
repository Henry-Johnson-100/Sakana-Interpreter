module TreeInterpreter.LocalCheck.TreeIs
  ( sendingValueBinding,
    primitiveValueBinding,
    primitivelyEvaluable,
    functionCall,
    simpleValueBindingCall,
    executable,
    positionalArg,
    standardLibCall,
    swim,
    storeable,
    symbolValueBinding,
  )
where

import qualified Data.Maybe as DMaybe (fromJust, maybe)
import qualified Lexer (Token (Keyword), baseData)
import qualified SyntaxTree (SyntaxTree, SyntaxUnit (context, token))
import qualified Token.Bracket as B (ScopeType (Return, Send))
import qualified Token.Data as D (fromData)
import qualified Token.Keyword as K (Keyword (Swim))
import qualified TreeInterpreter.LocalCheck.NodeIs as Check.NodeIs
  ( dataToken,
    dataTokenAndPrimitive,
    declarationRequiringId,
    fin,
    idNode,
    nullNode,
    operator,
  )
import qualified Util.General (foldIdApplicativeOnSingleton, head')
import qualified Util.Tree as Tree
  ( Tree (Empty),
    maybeOnTreeNode,
    nodeStrictlySatisfies,
    treeChildren,
    treeNode,
  )

-- | For a fish like >(some_id <(***)<)>
-- Where some_id should then be bound to the value *** in whatever scope immediately
-- follows.
sendingValueBinding :: SyntaxTree.SyntaxTree -> Bool
sendingValueBinding tr =
  symbolValueBinding tr
    && Tree.nodeStrictlySatisfies ((B.Send ==) . SyntaxTree.context) tr

-- | ditto for this, I don't like it that much
primitiveValueBinding :: SyntaxTree.SyntaxTree -> Bool
primitiveValueBinding =
  Util.General.foldIdApplicativeOnSingleton
    all
    [ symbolValueBinding,
      Tree.nodeStrictlySatisfies Check.NodeIs.idNode,
      primitivelyEvaluable . head . Tree.treeChildren
    ]

primitivelyEvaluable :: SyntaxTree.SyntaxTree -> Bool
primitivelyEvaluable =
  Util.General.foldIdApplicativeOnSingleton
    any
    ( [Tree.nodeStrictlySatisfies]
        <*> [ Check.NodeIs.operator,
              Check.NodeIs.fin,
              Check.NodeIs.dataTokenAndPrimitive
            ]
    )

-- | A tree is a function call if and only if the base node is an id
-- and it has no return children.
functionCall :: SyntaxTree.SyntaxTree -> Bool
functionCall tr =
  Tree.nodeStrictlySatisfies Check.NodeIs.idNode tr
    && hasNoReturnChildren tr
  where
    hasNoReturnChildren =
      null
        . filter (Tree.maybeOnTreeNode True ((B.Return ==) . SyntaxTree.context))
        . Tree.treeChildren

simpleValueBindingCall :: SyntaxTree.SyntaxTree -> Bool
simpleValueBindingCall tr =
  functionCall tr
    && ( null
           . filter (not . Tree.nodeStrictlySatisfies Check.NodeIs.nullNode)
           . Tree.treeChildren
       )
      tr

executable :: SyntaxTree.SyntaxTree -> Bool
executable Tree.Empty = False
executable tr =
  contextIsReturn tr
    && (functionCall tr || primitivelyEvaluable tr)
  where
    contextIsReturn = Tree.nodeStrictlySatisfies ((B.Return ==) . SyntaxTree.context)

positionalArg :: SyntaxTree.SyntaxTree -> Bool
positionalArg tr =
  (null . Tree.treeChildren) tr
    && Tree.nodeStrictlySatisfies ((B.Send ==) . SyntaxTree.context) tr

standardLibCall :: SyntaxTree.SyntaxTree -> Bool
standardLibCall tr =
  Tree.nodeStrictlySatisfies Check.NodeIs.dataToken tr
    && DMaybe.maybe False funcIdInStdLibList (Tree.treeNode tr)
  where
    funcIdInStdLibList =
      flip elem ["trout", "dolphin", "read", "floor"]
        . D.fromData
        . DMaybe.fromJust
        . Lexer.baseData
        . SyntaxTree.token

swim :: SyntaxTree.SyntaxTree -> Bool
swim tr =
  Tree.nodeStrictlySatisfies ((Lexer.Keyword (K.Swim) ==) . SyntaxTree.token) tr

-- | Can be stored in a symbol table.
--  As of right now, storeable and executable are not opposites.
--  because an anonymous function definition is not storeable
--  yet it is also not executable
--  but, named lambda functions: 'x <( >(m)> <(+ >(m)> >(1)>)<' for instance,
--  are storeable and should be stored as normal value bindings.
storeable :: SyntaxTree.SyntaxTree -> Bool
storeable = Tree.nodeStrictlySatisfies Check.NodeIs.declarationRequiringId

-- | For fish code that looks like:
-- 'some_id <(***)<'
-- where '***' is some wildcard value
-- I would like to not have this as a feature in the language to be honest.
symbolValueBinding :: SyntaxTree.SyntaxTree -> Bool
symbolValueBinding tr =
  Tree.nodeStrictlySatisfies Check.NodeIs.idNode tr
    && firstChildIsReturnContext tr
  where
    firstChildIsReturnContext tr =
      case ((Util.General.head' . Tree.treeChildren) tr) >>= Tree.treeNode of
        Nothing -> False
        Just x -> ((B.Return ==) . SyntaxTree.context) x