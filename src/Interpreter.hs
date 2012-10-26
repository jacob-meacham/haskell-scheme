module Main where
import System.Environment
import System.IO
import Control.Monad
import Language.Scheme.Types
import Language.Scheme.Environment
import Language.Scheme.Parser
import Language.Scheme.Eval
import Language.Scheme.Primitives

-- Default environment
primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc PrimitiveFunc) primitives
                                               ++ map (makeFunc IOFunc) ioPrimitives)
    where makeFunc constructor (var, func) = (var, constructor func)

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
    result <- prompt
    if pred result
        then return ()
        else action result >> until_ pred prompt action

runOne :: [String] -> IO ()
runOne args = do
            env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
            result <- runIOThrows $ liftM show $ eval env (List [Atom "load", String $ args !! 0])
            hPutStrLn stderr result

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Scheme>>> ") . evalAndPrint

main :: IO ()
main = do
    args <- getArgs
    case length args of
        0 -> runRepl
        otherwise -> runOne args

