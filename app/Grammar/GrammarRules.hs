module Grammar.GrammarRules (
GrammarRule(..),
rule,
isValidGroupedTokenGrammar
) where

import Lexer
import Token.Bracket
import Token.Keyword 
import Token.Control
import Token.Data
import Token.Util.NestedCollapsible
import Exception.GrammarException


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
    idRequired :: Bool,
    sendRules   :: [BracketRule],
    returnRules :: [BracketRule]
}


instance Show GrammarRule where
    show NoGR = "NoGR"
    show gr = unwords $ ["Type:", show  (grType gr), "\nRequires ID:", show (idRequired gr) ,"\n\nsend Rules:"] ++ map show (sendRules gr) ++ ["\n\nreturn Rules:"] ++ map show (returnRules gr)


instance Eq GrammarRule where
    (==) NoGR NoGR = True
    (==) grx  gry  = (grType grx) == (grType gry) && (sendRules grx) == (sendRules gry) && (returnRules grx) == (returnRules gry) && (idRequired grx) == (idRequired gry)
    (/=) grx  gry  = not (grx == gry)


rule :: Token -> GrammarRule
rule (Keyword Fish)   = GrammarRule (Keyword Fish)   True  [generalOpenBRRule]                                   [generalCloseBRRule]
rule (Keyword Route)  = GrammarRule (Keyword Route)  True  [generalOpenBRRule]                                   [generalCloseBRRule]
rule (Keyword School) = GrammarRule (Keyword School) True  [flipIsRequired generalOpenBRRule, generalOpenBRRule] [generalCloseBRRule]
rule (Keyword Shoal)  = GrammarRule (Keyword Shoal)  True  [setArgSets generalOpenBRRule 0]                      [generalCloseBRRule]
rule (Control Fin)    = GrammarRule (Control Fin)    False [flipIsRequired (setArgSets generalOpenBRRule 3)]     [(flipIsRequired (setArgSets generalCloseBRRule 0))]
rule _ = NoGR


grammarRuleRequiredGroupLength :: GrammarRule -> Int
grammarRuleRequiredGroupLength gr = 1 + (length (sendRules gr)) + (length (returnRules gr))


isValidGroupBracketGrammar :: BracketRule -> [Token] -> Bool
isValidGroupBracketGrammar br ts
    | not (isCompleteNestedCollapsible (ncCase br) ts) = False
    | argsRequired br && (argSets br) == 0           = null (getNestedCollapsibleContents (ncCase br) ts)
    | argsRequired br                                = completeArgumentSet (getNestedCollapsibleContents (ncCase br) ts) ((argSets br) - 1)
    | otherwise                                      = True
    where 
        break' = break ((Data (Punct ",")) ==) ts
        first  = fst break'
        second = snd break'
        completeArgumentSet :: [Token] -> Int -> Bool
        completeArgumentSet ts 0 = not (null ts) && null second --Too many arguments if False
        completeArgumentSet ts commasRemaining
            | null ts     = False --Not enough arguments
            | null first  = False
            | otherwise = completeArgumentSet (tail second) (commasRemaining - 1)


isValidGroupedTokenGrammar :: GrammarRule -> [[Token]] -> Bool
isValidGroupedTokenGrammar gr tts
    | grammarRuleRequiredGroupLength gr /= length tts             = False
    | length (head tts) /= (1 + (if idRequired gr then 1 else 0)) = False
    | otherwise                                                   = all (\any -> any) (map (\index -> isValidGroupBracketGrammar (((sendRules gr) ++ (returnRules gr)) !! index) (tts !! index)) [1..(length tts)])