module Language.Scheme.Eval where
import Control.Monad.Error
import Data.IORef
import Language.Scheme.Types
import Language.Scheme.Functions
import Language.Scheme.Environment

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(Character _) = return val
eval env val@(String _) = return val
eval env (Atom id) = getVar env id
eval env val@(Bool _) = return val
eval env val@(Number _) = return val
eval env val@(Float _) = return val
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) =
    do
    result <- eval env pred
    case result of
        Bool False -> eval env alt
        Bool True -> eval env conseq
        _ -> throwError $ TypeMismatch "bool" pred
eval env (List (Atom "cond" : firstCond : rest)) = evalConditional env firstCond rest
eval env (List (Atom "case" : key : firstClause : rest)) = do
                                                        keyResult <- eval env key
                                                        evalCase env key firstClause rest
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
    makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
    makeVarargs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
    makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
    makeVarargs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
    makeVarargs varargs env [] body
eval env (List (func : args)) = do
                                evaledFunc <- eval env func
                                argVals <- mapM (eval env) args
                                apply evaledFunc argVals
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

evalConditional :: Env -> LispVal -> [LispVal] -> IOThrowsError LispVal
evalConditional env (List (cond : exprs)) (x:xs) = do
                                            result <- eval env cond
                                            case result of
                                                Bool False -> evalConditional env x xs
                                                Bool True -> evalMultipleExpressions env exprs
                                                otherwise -> throwError $ TypeMismatch "Boolean" cond
evalConditional env (List (Atom "else" : exprs)) [] = evalMultipleExpressions env exprs
evalConditional env (List (cond : exprs)) [] = do
                                        result <- eval env cond
                                        case result of
                                            Bool True -> evalMultipleExpressions env exprs
                                            otherwise -> throwError $ Default "no conditional evaluated to true. Undefined behavior."
evalConditional env (nonList) _ = throwError $ TypeMismatch "List" nonList

-- Evaluates all the expressions and returns the last.
evalMultipleExpressions :: Env -> [LispVal] -> IOThrowsError LispVal
evalMultipleExpressions _ [] = throwError $ Default "No expressions to evaluate. Undefined behavior."
evalMultipleExpressions env exprs = do
                            result <- mapM (eval env) exprs
                            return $ last result

checkCaseKey :: Env -> LispVal -> [LispVal] -> IOThrowsError Bool
checkCaseKey env key values = do
                        results <- mapM (eval env) values
                        liftM or (mapM (liftThrows . eqv key) results)

evalCase :: Env -> LispVal -> LispVal -> [LispVal] -> IOThrowsError LispVal
evalCase env key (List (List datas : exprs)) (x : xs) = do
                                        caseSatisfied <- checkCaseKey env key datas
                                        case caseSatisfied of
                                            True ->  evalMultipleExpressions env exprs
                                            False -> evalCase env key x xs
evalCase env _ (List (Atom "else" : exprs)) [] = evalMultipleExpressions env exprs
evalCase env key (List (List datas : exprs)) [] = do
                                        caseSatisfied <- checkCaseKey env key datas
                                        case caseSatisfied of
                                            True ->  evalMultipleExpressions env exprs
                                            otherwise -> throwError $ Default "no case evaluated to true. Undefined behavior."
evalCase env (List (nonList : _)) _ _ = throwError $ TypeMismatch "List" nonList
evalCase env (nonList) _ _ = throwError $ TypeMismatch "List" nonList

-- Function application
-- monadic apply
apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (IOFunc func) args = func args
apply (Func params varargs body closure) args
    | num params /= num args && varargs == Nothing = throwError $ NumArgs (num params) args
    | otherwise = do
        argEnv <- (liftIO $ bindVars closure $ zip params args)
        finalEnv <- bindVarArgs varargs argEnv
        evalBody finalEnv
    where remainingArgs = drop (length params) args
          num = toInteger . length
          evalBody env = liftM last $ mapM (eval env) body
          bindVarArgs arg env = case arg of
            Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
            Nothing -> return env
apply val args = throwError $ TypeMismatch "Func" val

-- non-monadic apply
--apply :: String -> [LispVal] -> LispVal
--apply func args = maybe (Bool False) ($ args) $ lookup func primitives

--apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function " func)
--                        ($ args)
--                        (lookup func primitives)

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarargs = makeFunc . Just . showVal

-- Given two LispVals, returns their equivalence.
eqv :: LispVal -> LispVal -> ThrowsError Bool
eqv (Character a) (Character b) = return $ (a == b)
eqv (String a) (String b) = return $ (a == b)
eqv (Atom a) (Atom b) = return $ (a == b)
eqv (Number a) (Number b) = return $ (a == b)
eqv (Bool a) (Bool b) = return $ (a == b)
eqv (DottedList xs x) (DottedList ys y) = eqv (List $ xs ++ [x]) (List $ ys ++ [y])
eqv (List a) (List b) = return $ (length a == length b) &&
                                        (all eqvPair $ zip a b)
    where eqvPair (a, b) = case eqv a b of
                                Left err -> False
                                Right val -> val
eqv _ _ = return $ False
