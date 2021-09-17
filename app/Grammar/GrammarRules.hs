module Grammar.GrammarRules (
GrammarRule(..),
rule
) where

import Lexer
import Token.Bracket
import Token.Keyword 
import Token.Util.NestedCollapsible


data BracketRule = BracketRule {
    brType     :: Bracket,
    isRequired :: Bool,
    numSets    :: Int,
    ncCase     :: NCCase Token,
    argSets    :: Int
}


instance Show BracketRule where
    show br = unwords ["\nType:",show (brType br),"\n\tis required:", show (isRequired br), "\n\tNumber of bracket sets:", show (numSets br)]


instance Eq BracketRule where
    (==) (BracketRule brtypex isRx numSetsx _ argSetsx) (BracketRule brtypey isRy numSety _ argSetsy) = brtypex == brtypey && isRx == isRy && numSetsx == numSety && argSetsx == argSetsy
    (/=) brx                                   bry                                  = not (brx == bry)


generalOpenBRRule :: BracketRule
generalOpenBRRule = BracketRule {
        brType     = Send Open,
        isRequired = False,
        numSets    = 1,
        ncCase     = NCCase ((Bracket (Send Open)) == ) ((Bracket (Send Close)) == ),
        argSets    = -1
    }


generalCloseBRRule :: BracketRule
generalCloseBRRule = BracketRule {
        brType     = Return Open,
        isRequired = True,
        numSets    = 1,
        ncCase     = NCCase ((Bracket (Return Open)) == ) ((Bracket (Return Close)) == ),
        argSets    = -1
    }


flipIsRequired :: BracketRule -> BracketRule
flipIsRequired br = br {isRequired = not (isRequired br)}


setNumSets :: BracketRule -> Int -> BracketRule
setNumSets br n = br {numSets = n}


setArgSets :: BracketRule -> Int -> BracketRule
setArgSets br n = br {argSets = n}


argsRequired :: BracketRule -> Bool
argsRequired br = (argSets br) >= 0


data GrammarRule = NoGR | GrammarRule {
    grType     :: Token,
    sendRule   :: BracketRule,
    returnRule :: BracketRule
}


instance Show GrammarRule where
    show gr = unwords ["Type:", show  (grType gr), "\n\nsend Rule:", show (sendRule gr), "\n\nreturn Rule:", show (returnRule gr)]


instance Eq GrammarRule where
    (==) NoGR NoGR = True
    (==) grx  gry  = (grType grx) == (grType gry) && (sendRule grx) == (sendRule gry) && (returnRule grx) == (returnRule gry)
    (/=) grx  gry  = not (grx == gry)


rule :: Token -> GrammarRule
rule (Keyword Fish)   = GrammarRule (Keyword Fish)   generalOpenBRRule                                 generalCloseBRRule
rule (Keyword Route)  = GrammarRule (Keyword Route)  generalOpenBRRule                                 generalCloseBRRule
rule (Keyword School) = GrammarRule (Keyword School) (flipIsRequired (setNumSets generalOpenBRRule 2)) generalCloseBRRule
rule (Keyword Shoal)  = GrammarRule (Keyword Shoal)  (flipIsRequired (setNumSets generalOpenBRRule 0)) generalCloseBRRule
rule _ = NoGR