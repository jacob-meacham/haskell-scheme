module Language.Scheme.Primitives where
import Control.Monad.Error
import Language.Scheme.Types
import Language.Scheme.Eval

-- Built-in functions
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("eqv?", eqv'),
              ("eq?", eqv'),
              ("equal?", equal),
              ("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("make-string", makeString),
              ("string-length", stringLength),
              ("string-ref", stringRef),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("not", unaryOp builtinNot),
              ("string?", unaryOp isString),
              ("symbol?", unaryOp isSymbol),
              ("boolean?", unaryOp isBoolean),
              ("number?", unaryOp isNumber),
              ("string->symbol", stringToSymbol),
              ("symbol->string", symbolToString),
              ("cons", cons),
              ("cdr", cdr),
              ("car", car)]

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

eqv' :: [LispVal] -> ThrowsError LispVal
eqv' [a, b] = do
                result <- eqv a b
                case result of
                    True -> return $ Bool True
                    False -> return $ Bool False
eqv' incorrectNum = throwError $ NumArgs 2 incorrectNum

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

-- do block will return false if there is an error, and we pass that and
-- a const function to catchError
unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals a b (AnyUnpacker unpacker) = do
                                        unpackA <- unpacker a
                                        unpackB <- unpacker b
                                        return $ unpackA == unpackB
                                    `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [a, b] = do
            primitiveEquals <- liftM or $ mapM (unpackEquals a b)
                                                [AnyUnpacker unpackNum, AnyUnpacker unpackString, AnyUnpacker unpackBool]
            eqvEquals <- eqv' [a, b]
            return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal incorrectNum = throwError $ NumArgs 2 incorrectNum

-- non-monadic
-- numericBinop op params = Number $ foldl1 op $ map unpackNum params
numericBinop :: (Int -> Int -> Int) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                            then throwError $ NumArgs 2 args
                            else do left <- unpacker $ args !! 0
                                    right <- unpacker $ args !! 1
                                    return $ Bool $ left `op` right

strBoolBinop = boolBinop unpackString
numBoolBinop = boolBinop unpackNum
boolBoolBinop = boolBinop unpackBool

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

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp func [x] = return $ func x
unaryOp _ list = throwError $ NumArgs 1 list

isString :: LispVal -> LispVal
isString (String _) = Bool True
isString _ = Bool False

isSymbol :: LispVal -> LispVal
isSymbol (Atom _) = Bool True
isSymbol _ = Bool False

isBoolean :: LispVal -> LispVal
isBoolean (Bool _) = Bool True
isBoolean _ = Bool False

isNumber :: LispVal -> LispVal
isNumber (Number _) = Bool True
isNumber _ = Bool False

isList :: LispVal -> LispVal
isList (List _) = Bool True
isList (DottedList _ _) = Bool True
isList _ = Bool False

builtinNot :: LispVal -> LispVal
builtinNot (Bool b) = Bool $ not b
builtinNot _ = Bool False

symbolToString :: [LispVal] -> ThrowsError LispVal
symbolToString [Atom s] = return $ String s
symbolToString [nonAtom] = throwError $ TypeMismatch "Atom" nonAtom
symbolToString multiList = throwError $ NumArgs 1 multiList

-- String primitives
-- TODO: I should be able to use these in unaryOp
stringToSymbol :: [LispVal] -> ThrowsError LispVal
stringToSymbol [String s] = return $ Atom s
stringToSymbol [nonString] = throwError $ TypeMismatch "String" nonString
stringToSymbol multiList = throwError $ NumArgs 1 multiList

makeString :: [LispVal] -> ThrowsError LispVal
makeString [Number num] = return $ String $ take num $ repeat ' '
makeString [Number num, Character char] = return $ String $ take num $ repeat char
makeString [Number num, nonCharacter] = throwError $ TypeMismatch "Character" nonCharacter
makeString [nonNumber, _] = throwError $ TypeMismatch "Number" nonNumber
makeString multiList = throwError $ NumArgs 2 multiList

stringLength :: [LispVal] -> ThrowsError LispVal
stringLength [String s] = return $ Number $ length s
stringLength [nonString] = throwError $ TypeMismatch "String" nonString
stringLength multiList = throwError $ NumArgs 1 multiList

stringRef :: [LispVal] -> ThrowsError LispVal
stringRef [String s, Number n]
            | n > length s = throwError $ Default $ "Out of bounds on string ref for string: " ++ s ++ " index: " ++ show n
            | otherwise = return $ Character $ s !! n
stringRef [String s, nonNumber] = throwError $ TypeMismatch "Number" nonNumber
stringRef [nonString, _] = throwError $ TypeMismatch "String" nonString
stringRef multiList = throwError $ NumArgs 2 multiList

-- List primitives
car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)] = return x
car [DottedList (x:xs) _] = return x
car [nonList] = throwError $ TypeMismatch "List" nonList
car multiList = throwError $ NumArgs 1 multiList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)] = return $ List xs
cdr [DottedList [_] end] = return end
cdr [DottedList (head:tail) end] = return $ DottedList tail end
cdr [nonList] = throwError $ TypeMismatch "List" nonList
cdr multiList = throwError $ NumArgs 1 multiList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = return $ List [x]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs tail] = return $ DottedList (x : xs) tail
cons [x, y] = return $ DottedList [x] y
cons multiList = throwError $ NumArgs 2 multiList

-- IO Primtives
