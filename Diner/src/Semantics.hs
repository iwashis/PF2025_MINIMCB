{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Semantics where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (SomeException, catch)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (intercalate, sort)
import Parser
import System.Random
import Text.Read (readMaybe)

-- Runtime values
data Value
    = VInt Int
    | VString String
    | VResource (TMVar ()) -- STM transactional variable for resource locking
    deriving (Eq)

instance Show Value where
    show (VInt n) = show n
    show (VString s) = s
    show (VResource _) = "<resource>"

-- Process state containing variable bindings
type Environment = Map String Value

-- ANSI color codes for different threads
data ThreadColor = Red | Green | Yellow | Blue | Magenta | Cyan | White
    deriving (Eq, Show, Enum, Bounded)

colorCode :: ThreadColor -> String
colorCode Red = "\ESC[31m"
colorCode Green = "\ESC[32m"
colorCode Yellow = "\ESC[33m"
colorCode Blue = "\ESC[34m"
colorCode Magenta = "\ESC[35m"
colorCode Cyan = "\ESC[36m"
colorCode White = "\ESC[37m"

resetCode :: String
resetCode = "\ESC[0m"

-- Global state containing all resources and processes - now shared via TVar
data GlobalState = GlobalState
    { globalResources :: Map String (TMVar ())
    , processCounter :: Int
    , threadColors :: Map ThreadId ThreadColor
    }
    deriving (Eq)

-- Shared global state
type SharedGlobalState = TVar GlobalState

-- Evaluation monad that uses shared global state
type EvalM = ReaderT SharedGlobalState IO

-- Initialize empty global state
initialState :: GlobalState
initialState = GlobalState Map.empty 0 Map.empty

-- Run the evaluator with shared state
runEval :: SharedGlobalState -> EvalM a -> IO a
runEval sharedState action = runReaderT action sharedState

-- Get current resource status as a formatted string with real-time lock checking
getResourceStatusString :: EvalM String
getResourceStatusString = do
    sharedState <- ask
    globalState <- liftIO $ readTVarIO sharedState
    let resourceMap = globalResources globalState
    
    -- Check actual TMVar states for each resource
    statusList <- liftIO $ mapM checkResourceStatus (Map.toList resourceMap)
    let sortedStates = sort statusList
    
    return $ if null sortedStates 
             then "Resources: []"
             else "Resources: [" ++ intercalate ", " sortedStates ++ "]"
  where
    checkResourceStatus (name, tmvar) = do
        -- Try to read the TMVar without blocking to check if it's available
        isEmpty <- atomically $ isEmptyTMVar tmvar
        return $ name ++ ":" ++ (if isEmpty then "🔒" else "✅")

-- Expression evaluation with better error handling
evalExpr :: Environment -> Expr -> EvalM Value
evalExpr env expr = case expr of
    Var name ->
        case Map.lookup name env of
            Just val -> return val
            Nothing -> liftIO $ do
                putStrLn $ "Error: Undefined variable: " ++ name
                return $ VString ("<undefined:" ++ name ++ ">")
    StringLit s -> return $ VString s
    IntLit n -> return $ VInt n
    Concat e1 e2 -> do
        v1 <- evalExpr env e1
        v2 <- evalExpr env e2
        return $ VString (valueToString v1 ++ valueToString v2)
    Rand e1 e2 -> do
        v1 <- evalExpr env e1
        v2 <- evalExpr env e2
        case (v1, v2) of
            (VInt lower, VInt upper) -> liftIO $ do
                gen <- newStdGen
                let (val, _) = randomR (lower, upper) gen
                return $ VInt val
            _ -> liftIO $ do
                putStrLn "Error: rand requires integer arguments"
                return $ VInt 0
    Add e1 e2 -> do
        v1 <- evalExpr env e1
        v2 <- evalExpr env e2
        case (v1, v2) of
            (VInt n1, VInt n2) -> return $ VInt (n1 + n2)
            _ -> liftIO $ do
                putStrLn "Error: + requires integer operands"
                return $ VInt 0
    Sub e1 e2 -> do
        v1 <- evalExpr env e1
        v2 <- evalExpr env e2
        case (v1, v2) of
            (VInt n1, VInt n2) -> return $ VInt (n1 - n2)
            _ -> liftIO $ do
                putStrLn "Error: - requires integer operands"
                return $ VInt 0
    Mod e1 e2 -> do
        v1 <- evalExpr env e1
        v2 <- evalExpr env e2
        case (v1, v2) of
            (VInt n1, VInt n2) | n2 /= 0 -> return $ VInt (n1 `mod` n2)
            (VInt _, VInt 0) -> liftIO $ do
                putStrLn "Error: division by zero in mod operation"
                return $ VInt 0
            _ -> liftIO $ do
                putStrLn "Error: % requires integer operands"
                return $ VInt 0

-- Convert value to string representation
valueToString :: Value -> String
valueToString (VInt n) = show n
valueToString (VString s) = s
valueToString (VResource _) = "<resource>"

-- Convert value to integer (for conditions)
valueToBool :: Value -> Bool
valueToBool (VInt 0) = False
valueToBool (VInt _) = True
valueToBool (VString "") = False
valueToBool (VString _) = True
valueToBool (VResource _) = True

-- Safe resource lookup in environment
lookupResource :: String -> Environment -> Maybe (TMVar ())
lookupResource name env = case Map.lookup name env of
    Just (VResource tmvar) -> Just tmvar
    _ -> Nothing

-- Update global state
updateGlobalState :: (GlobalState -> GlobalState) -> EvalM ()
updateGlobalState f = do
    sharedState <- ask
    liftIO $ atomically $ modifyTVar sharedState f

-- Safe resource lookup in global state
lookupGlobalResource :: String -> EvalM (Maybe (TMVar ()))
lookupGlobalResource name = do
    sharedState <- ask
    globalState <- liftIO $ readTVarIO sharedState
    return $ Map.lookup name (globalResources globalState)

-- Get or assign a color for the current thread
getThreadColor :: EvalM ThreadColor
getThreadColor = do
    tid <- liftIO myThreadId
    sharedState <- ask
    globalState <- liftIO $ readTVarIO sharedState

    case Map.lookup tid (threadColors globalState) of
        Just color -> return color
        Nothing -> do
            -- Assign a new color (cycling through available colors)
            let availableColors = [Red, Green, Yellow, Blue, Magenta, Cyan, White]
            let usedColors = Map.elems (threadColors globalState)
            let nextColor = case filter (`notElem` usedColors) availableColors of
                    (c : _) -> c
                    [] -> availableColors !! (Map.size (threadColors globalState) `mod` length availableColors)

            -- Update global state with new color assignment
            liftIO $ atomically $ modifyTVar sharedState $ \s ->
                s{threadColors = Map.insert tid nextColor (threadColors s)}

            return nextColor

-- Enhanced colored print function with resource monitoring
colorPrint :: String -> EvalM ()
colorPrint msg = do
    tid <- liftIO myThreadId
    color <- getThreadColor
    resourceStatus <- getResourceStatusString
    let tidStr = take 8 $ show tid -- Truncate thread ID for readability
    liftIO $ putStrLn $ colorCode color ++ "[" ++ tidStr ++ "] " ++ msg ++ resetCode ++ " | " ++ resourceStatus

-- Statement execution with improved error handling
execStmt :: Environment -> Statement -> EvalM Environment
execStmt env stmt = do
    sharedState <- ask
    result <-
        liftIO $
            (runEval sharedState $ execStmt' env stmt) `catch` \(e :: SomeException) -> do
                putStrLn $ "Runtime error: " ++ show e
                return env
    return result

execStmt' :: Environment -> Statement -> EvalM Environment
execStmt' environment statement = case statement of
    Think expr -> do
        val <- evalExpr environment expr
        case val of
            VInt duration -> do
                colorPrint $ "Thinking for " ++ show duration ++ " time units"
                liftIO $ threadDelay (duration * 1000) -- Convert to microseconds
                return environment
            _ -> do
                colorPrint "Error: think requires integer argument"
                return environment
    Eat expr (Resource r1Name) (Resource r2Name) -> do
        val <- evalExpr environment expr
        case val of
            VInt duration -> do
                -- Look up resources in environment (they should be bound variables)
                case (lookupResource r1Name environment, lookupResource r2Name environment) of
                    (Just _, Just _) -> do
                        colorPrint $ "Eating for " ++ show duration ++ " time units using resources " ++ r1Name ++ " and " ++ r2Name
                        liftIO $ threadDelay (duration * 1000)
                        return environment
                    _ -> do
                        colorPrint $ "Error: Resources not found in environment: " ++ r1Name ++ ", " ++ r2Name
                        return environment
            _ -> do
                colorPrint "Error: eat requires integer duration"
                return environment
    PrintExpr expr -> do
        val <- evalExpr environment expr
        colorPrint $ valueToString val
        return environment
    DeclareResource expr -> do
        val <- evalExpr environment expr
        case val of
            VString name -> do
                resource <- liftIO $ newTMVarIO ()
                updateGlobalState $ \s -> s { 
                    globalResources = Map.insert name resource (globalResources s)
                }
                colorPrint $ "Declared resource: " ++ name
                return environment
            _ -> do
                colorPrint "Error: declareResource requires string argument"
                return environment
    Loop stmts -> do
        sharedState <- ask
        liftIO $ forkIO $ runLoop sharedState environment stmts
        return environment
      where
        runLoop shared env statements = do
            newEnv <- runEval shared $ execStmts env statements
            runLoop shared newEnv statements
    Spawn expr stmts -> do
        val <- evalExpr environment expr
        case val of
            VString processName -> do
                sharedState <- ask
                colorPrint $ "Spawning process: " ++ processName
                liftIO $ do
                    forkIO $ do
                        _ <- runEval sharedState $ execStmts environment stmts
                        -- runEval sharedState $ colorPrint $ "Process " ++ processName ++ " terminated"
                        return ()
                    return ()
                return environment
            _ -> do
                colorPrint "Error: spawn requires string process name"
                return environment
    LockAll exprs varNames -> do
        -- Evaluate resource expressions to get resource names
        vals <- mapM (evalExpr environment) exprs
        let resourceNames = [name | VString name <- vals]

        if length resourceNames /= length vals
            then do
                colorPrint "Error: All resource expressions must evaluate to strings"
                return environment
            else do
                -- Look up actual TMVar resources from global state
                maybeResources <- mapM lookupGlobalResource resourceNames

                case sequence maybeResources of
                    Just resources -> do
                        -- Atomically acquire all resources
                        liftIO $ atomically $ mapM_ takeTMVar resources

                        -- Bind resources to variable names in environment
                        let resourceValues = map VResource resources
                        let newBindings = Map.fromList $ zip varNames resourceValues
                        let newEnv = Map.union newBindings environment

                        colorPrint $ "Locked resources: " ++ show resourceNames -- ++ " as variables: " ++ show varNames
                        return newEnv
                    Nothing -> do
                        colorPrint $ "Error: Some resources not found in global state: " ++ show resourceNames
                        return environment
    UnlockAll resources -> do
        -- Look up resource TMVars in environment and release them
        let resourceNames = [name | Resource name <- resources]
        let maybeResources = map (\name -> lookupResource name environment) resourceNames

        case sequence maybeResources of
            Just tmvars -> do
                liftIO $ atomically $ mapM_ (`putTMVar` ()) tmvars
                actualResourceNames <- mapM getActualResourceName resourceNames
                colorPrint $ "Unlocked resources: " ++ show actualResourceNames
                -- colorPrint $ "Unlocked resources: " ++ show resourceNames
                return environment
            Nothing -> do
                colorPrint $ "Error: Some resources not found in environment: " ++ show resourceNames
                return environment
    Let varName expr -> do
        val <- evalExpr environment expr
        return $ Map.insert varName val environment
    ForEach start end varName stmts -> do
        foldM
            ( \currentEnv i -> do
                let loopEnv = Map.insert varName (VInt i) currentEnv
                execStmts loopEnv stmts
            )
            environment
            [start .. end]
    If condExpr thenStmts elseStmts -> do
        condVal <- evalExpr environment condExpr
        if valueToBool condVal
            then execStmts environment thenStmts
            else execStmts environment elseStmts
    where 
      getActualResourceName varName = do
        -- Look through global resources to find which one matches this TMVar
        sharedState <- ask
        globalState <- liftIO $ readTVarIO sharedState
        case lookupResource varName environment of
            Just tmvar -> 
                case [name | (name, res) <- Map.toList (globalResources globalState), res == tmvar] of
                    (actualName:_) -> return actualName
                    [] -> return varName -- fallback
            Nothing -> return varName -- fallback

-- Execute a list of statements
execStmts :: Environment -> [Statement] -> EvalM Environment
execStmts = foldM execStmt

-- Execute a complete program
execProgram :: Program -> EvalM ()
execProgram (Program stmts) = do
    liftIO $ putStrLn "=== Starting program execution ==="
    _ <- execStmts Map.empty stmts
    liftIO $ putStrLn "=== Main program execution completed ==="
    -- Keep main thread alive to let spawned processes run
    liftIO $ threadDelay 5000000 -- 5 seconds
    return ()

-- Convenience function to run a program
runProgram :: Program -> IO ()
runProgram program = do
    sharedState <- newTVarIO initialState
    runEval sharedState $ execProgram program

-- Debug function to print current environment
printEnv :: Environment -> IO ()
printEnv env = do
    putStrLn "Current environment:"
    mapM_ (\(k, v) -> putStrLn $ "  " ++ k ++ " = " ++ show v) (Map.toList env)

-- Helper function to evaluate and print expressions (for testing)
evalAndPrint :: String -> Expr -> IO ()
evalAndPrint description expr = do
    sharedState <- newTVarIO initialState
    result <- runEval sharedState $ evalExpr Map.empty expr
    putStrLn $ description ++ ": " ++ show result

-- Test the evaluator with simple expressions
testEvaluator :: IO ()
testEvaluator = do
    putStrLn "=== Testing Expression Evaluator ==="

    -- Test basic expressions
    evalAndPrint "Integer literal" (IntLit 42)
    evalAndPrint "String literal" (StringLit "Hello")
    evalAndPrint "Addition" (Add (IntLit 10) (IntLit 5))
    evalAndPrint "Subtraction" (Sub (IntLit 10) (IntLit 3))
    evalAndPrint "Modulo" (Mod (IntLit 10) (IntLit 3))
    evalAndPrint "String concatenation" (Concat (StringLit "Hello ") (StringLit "World"))

    -- Test random (will vary each run)
    putStrLn "\nTesting random expressions (results will vary):"
    evalAndPrint "Random 1-10" (Rand (IntLit 1) (IntLit 10))
    evalAndPrint "Random 1-10" (Rand (IntLit 1) (IntLit 10))
    evalAndPrint "Random 1-10" (Rand (IntLit 1) (IntLit 10))

    putStrLn "\n=== Testing Simple Program Execution ==="

    -- Test simple program
    let simpleProgram =
            Program
                [ Let "x" (IntLit 10)
                , Let "y" (IntLit 20)
                , Let "sum" (Add (Var "x") (Var "y"))
                , PrintExpr (Var "sum")
                , Think (IntLit 100)
                , PrintExpr (Concat (StringLit "The sum is: ") (Var "sum"))
                ]

    runProgram simpleProgram
