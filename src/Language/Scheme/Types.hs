module Language.Scheme.Types where
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Control.Monad.Error
import Data.IORef
import System.IO

-- Our environment is a list of tuples of the key to the value wrapped in an IORef.
type Env = IORef [(String, IORef LispVal)]

-- Values --
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Int
             | String String
             | Bool Bool
             | Character Char
             | Float Double
             | Port Handle
             | Unit () -- Returned by functions/special forms that have undefined return behavior.
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | IOFunc ([LispVal] -> IOThrowsError LispVal) -- Special type for primitives that perform IO.
             | Func {params :: [String], vararg :: (Maybe String),
                      body :: [LispVal], closure :: Env}

instance Show LispVal where show = showVal
showVal :: LispVal -> String
showVal (Character c) = "\'" ++ [c] ++ "\'"
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Unit _) = "()"
showVal (Atom name) = name
showVal (Port _) = "<IO port>"
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Float float) = show float
showVal (Number number) = show number
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (PrimitiveFunc _) = "<primitive_function>"
showVal (IOFunc _) = "<io_primitive_function>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
    "(lambda (" ++ unwords (map show args) ++
          (case varargs of
                Nothing -> ""
                Just arg -> " . " ++ arg) ++ ") ...)"

-- LispVal helper functions
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

unpackString :: LispVal -> ThrowsError String
unpackString (String val) = return val
unpackString noString = throwError $ TypeMismatch "string" noString

unpackNum :: LispVal -> ThrowsError Int
unpackNum (Number val) = return val
unpackNum (String val) = let parsed = reads val in
                            if null parsed
                            then throwError $ TypeMismatch "number" $ String val
                            else return $ fst $ parsed !! 0
unpackNum (List [val]) = unpackNum val
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackNumStrict :: LispVal -> ThrowsError Int
unpackNumStrict (Number val) = return val
unpackNumStrict notNum = throwError $ TypeMismatch "number" notNum

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "number" notBool

-- Error Type --
data LispError = NumArgs [Integer] [LispVal]
                | TypeMismatch String LispVal
                | Parser ParseError
                | BadSpecialForm String LispVal
                | NotFunction String String
                | UnboundVar String String
                | Default String

instance Show LispError where show = showError
instance Error LispError where
            noMsg = Default "An error has occured"
            strMsg = Default

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ (unwords $ map show expected)
                                    ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                    ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (Default message) = message

-- Our error Monads.
type ThrowsError = Either LispError
type IOThrowsError = ErrorT LispError IO

-- Error helper functions
-- Given a ThrowsError, lifts the value into an IOThrowsError.
-- If the value was an error, then we just rethrow, otherwise we return into IOThrowsError.
liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

-- Runs the action and returns the result as an IO string.
runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

-- Runs the action, capturing the error and converting it to a string if need be.
trapError :: (Show e, MonadError e m) => m String -> m String
trapError action = action `catchError` (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
