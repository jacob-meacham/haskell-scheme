module Language.Scheme.Parser where
import Numeric
import Control.Monad
import Control.Monad.Error
import Text.ParserCombinators.Parsec hiding (spaces)
import Language.Scheme.Types

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseCharacter :: Parser LispVal
parseCharacter = do
                string "#\\"
                value <- do {
                            x <- try (string "space" <|> string "newline")
                            ; return x
                            }
                            <|>
                            do {
                                x <- anyChar
                                ; notFollowedBy alphaNum
                                ; return [x]
                                }
                return $ Character $ case value of
                    "space" -> ' '
                    "newline" -> '\n'
                    otherwise -> (value !! 0)

parseString :: Parser LispVal
parseString = do
            char '"'
            x <- many $ escapedString <|> noneOf "\"\\"
            char '"'
            return $ String x

escapedString :: Parser Char
escapedString = do
                char '\\'
                x <- oneOf "\\\"nrt"
                return $ case x of
                    '\\' -> x
                    '"' -> x
                    'n' -> '\n'
                    'r' -> '\r'
                    't' -> '\t'

parseAtom :: Parser LispVal
parseAtom = do
            first <- letter <|> symbol
            rest <- many (letter <|> digit <|> symbol)
            let atom = first:rest
            return $ Atom atom

parseQuoted :: Parser LispVal
parseQuoted = do
            char '\''
            x <- parseExpr
            return $ List [Atom "quote", x]

parseBool :: Parser LispVal
parseBool = do
            char '#'
            x <- oneOf "tf"
            return $ case x of
                    't' -> Bool True
                    'f' -> Bool False

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseNumber' :: Parser LispVal
parseNumber' = do
            digits <- many1 digit
            return $ Number $ read digits

parseNumber'' :: Parser LispVal
parseNumber'' = many1 digit >>= \x -> return $ Number $ read x

parseFloat :: Parser LispVal
parseFloat = do
            wholePart <- many1 digit
            char '.'
            decimalPart <- many1 digit
            return $ Float $ fst . head $ readFloat (wholePart ++ "." ++ decimalPart)

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
                head <- endBy parseExpr spaces
                tail <- char '.' >> spaces >> parseExpr
                return $ DottedList head tail

parseExpr :: Parser LispVal
parseExpr = parseAtom
       <|> parseString
       <|> try parseFloat
       <|> try parseNumber'
       <|> try parseBool
       <|> try parseCharacter
       <|> parseQuoted
       <|> do
            char '('
            x <- try parseList <|> parseDottedList
            char ')'
            return x

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

readExpr = readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr spaces)

-- Load a file and read as an expression list
load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList
