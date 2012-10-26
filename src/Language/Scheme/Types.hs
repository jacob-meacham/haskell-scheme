module Language.Scheme.Types where
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Control.Monad.Error
import Data.IORef
import System.IO

type Env = IORef [(String, IORef LispVal)]

-- Value --
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Int
             | String String
             | Bool Bool
             | Character Char
             | Float Double
             | Port Handle
             | Unit ()
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

-- Error --
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

type ThrowsError = Either LispError
type IOThrowsError = ErrorT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

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

-- Error helper functions
trapError :: (Show e, MonadError e m) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

test :: [Integer] -> [String]
test x = map show x
