module Exception.GrammarException where

import Exception

newBracketException :: String -> Exception
newBracketException s =
  Exception
    { exceptionType = InvalidBracketing,
      information =
        ExceptionInfo
          { exceptionMessage = s,
            severity = Fatal
          }
    }

mismatchedBrackets :: Exception
mismatchedBrackets = newBracketException "Mismatched Brackets"

missingRequiredReturn :: Exception
missingRequiredReturn = newBracketException "Missing a Return set of brackets: <()<"

missingRequiredSend :: Exception
missingRequiredSend = newBracketException "Missing a Send set of brackets: >()>"