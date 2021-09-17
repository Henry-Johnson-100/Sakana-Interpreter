module Grammar.GrammarRules (
GrammarRule(..)
) where

import Lexer
import Token.Bracket (Bracket)
import Token.Util.NestedCollapsible (NCCase)


data BracketRule = BracketRule {
    brType     :: Bracket,
    isRequired :: Bool,
    numSets    :: Int,
    ncCase     :: NCCase Token
}


instance Show BracketRule where
    show br = unwords ["Type:",show (brType br),",","is required: ", show (isRequired br)]


instance Eq BracketRule where
    (==) (BracketRule brtypex isRx numSetsx _) (BracketRule brtypey isRy numSety _) = brtypex == brtypey && isRx == isRy && numSetsx == numSety
    (/=) brx                                   bry                                  = not (brx == bry)


data GrammarRule = NoGR | GrammarRule {
    grType     :: Token,
    sendRule   :: BracketRule,
    returnRule :: BracketRule
}


instance Show GrammarRule where
    show gr = unwords ["Type:", show  (grType gr), "send Rule:", show (sendRule gr), "return Rule:", show (returnRule gr)]


instance Eq GrammarRule where
    (==) NoGR NoGR = True
    (==) grx  gry  = (grType grx) == (grType gry) && (sendRule grx) == (sendRule gry) && (returnRule grx) == (returnRule gry)
    (/=) grx  gry  = not (grx == gry)


rule :: Token -> GrammarRule
rule _ = NoGR