module Grammar.GrammarRules (
GrammarRule(..),
rule
) where

import Lexer
import Token.Bracket
import Token.Keyword 
import Token.Control
import Token.Util.NestedCollapsible


data BracketRule = BracketRule {
    brType     :: Bracket,
    isRequired :: Bool,
    ncCase     :: NCCase Token,
    argSets    :: Int
}


instance Show BracketRule where
    show br = unwords ["\nType:",show (brType br),"\n\tis required:", show (isRequired br), "\n\tNumber of arguments:", (if argsRequired br then show (argSets br) else "Optional")]


instance Eq BracketRule where
    (==) (BracketRule brtypex isRx _ argSetsx) (BracketRule brtypey isRy _ argSetsy) = brtypex == brtypey && isRx == isRy && argSetsx == argSetsy
    (/=) brx                                   bry                                  = not (brx == bry)


generalOpenBRRule :: BracketRule
generalOpenBRRule = BracketRule {
        brType     = Send Open,
        isRequired = False,
        ncCase     = NCCase ((Bracket (Send Open)) == ) ((Bracket (Send Close)) == ),
        argSets    = -1
    }


generalCloseBRRule :: BracketRule
generalCloseBRRule = BracketRule {
        brType     = Return Open,
        isRequired = True,
        ncCase     = NCCase ((Bracket (Return Open)) == ) ((Bracket (Return Close)) == ),
        argSets    = 1
    }


flipIsRequired :: BracketRule -> BracketRule
flipIsRequired br = br {isRequired = not (isRequired br)}


setArgSets :: BracketRule -> Int -> BracketRule
setArgSets br n = br {argSets = n}


argsRequired :: BracketRule -> Bool
argsRequired br = (argSets br) >= 0


data GrammarRule = NoGR | GrammarRule {
    grType     :: Token,
    sendRules   :: [BracketRule],
    returnRules :: [BracketRule]
}


instance Show GrammarRule where
    show gr = unwords $ ["Type:", show  (grType gr), "\n\nsend Rule:"] ++ map show (sendRules gr) ++ ["\n\nreturn Rule:"] ++ map show (returnRules gr)


instance Eq GrammarRule where
    (==) NoGR NoGR = True
    (==) grx  gry  = (grType grx) == (grType gry) && (sendRules grx) == (sendRules gry) && (returnRules grx) == (returnRules gry)
    (/=) grx  gry  = not (grx == gry)


rule :: Token -> GrammarRule
rule (Keyword Fish)   = GrammarRule (Keyword Fish)   [generalOpenBRRule]                                   [generalCloseBRRule]
rule (Keyword Route)  = GrammarRule (Keyword Route)  [generalOpenBRRule]                                   [generalCloseBRRule]
rule (Keyword School) = GrammarRule (Keyword School) [flipIsRequired generalOpenBRRule, generalOpenBRRule] [generalCloseBRRule]
rule (Keyword Shoal)  = GrammarRule (Keyword Shoal)  [setArgSets generalOpenBRRule 0]                      [generalCloseBRRule]
rule (Control Fin)    = GrammarRule (Control Fin)    [flipIsRequired (setArgSets generalOpenBRRule 3)]     [(flipIsRequired (setArgSets generalCloseBRRule 0))]
rule _ = NoGR