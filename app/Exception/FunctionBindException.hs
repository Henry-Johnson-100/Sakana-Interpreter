module Exception.FunctionBindException where

import Exception

newFunctionBindArgException :: String -> Exception
newFunctionBindArgException s = Exception {
    exceptionType = InvalidArgs,
    information   = ExceptionInfo {
        exceptionMessage = s,
        severity         = Fatal
    }
}

newFunctionBindIDException :: String -> Exception
newFunctionBindIDException s = Exception {
    exceptionType = InvalidID,
    information   = ExceptionInfo {
        exceptionMessage = s,
        severity         = Fatal
    }
}

invalidFunctionName = newFunctionBindIDException "Invalid function name"

invalidFinArguments = newFunctionBindArgException "Invalid arguments for a Fin statement"

missingArguments = newFunctionBindArgException "Missing the required number of arguments"

excessiveArguments = newFunctionBindArgException "Too many arguments"