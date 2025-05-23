# Building an STM-Based Evaluator for the Dining Philosophers Language

This tutorial continues from our parser building and testing tutorials, where we created a parser for the Dining Philosophers language. Now we'll build a concurrent evaluator that can execute our parsed programs using Haskell's Software Transactional Memory (STM) system to handle the complex concurrency requirements of the dining philosophers problem.

## Table of Contents

1. [Understanding Concurrency in the Dining Philosophers Problem](#understanding-concurrency-in-the-dining-philosophers-problem)
2. [Introduction to Software Transactional Memory (STM)](#introduction-to-software-transactional-memory-stm)
3. [Understanding TVars: Transactional Variables](#understanding-tvars-transactional-variables)
4. [Project Setup and Dependencies](#project-setup-and-dependencies)
5. [Defining Runtime Values and State](#defining-runtime-values-and-state)
6. [Building the Evaluation Monad](#building-the-evaluation-monad)
7. [Implementing Expression Evaluation](#implementing-expression-evaluation)
8. [Implementing Statement Execution](#implementing-statement-execution)
9. [Advanced STM Patterns for Resource Management](#advanced-stm-patterns-for-resource-management)
10. [Testing the Evaluator](#testing-the-evaluator)
11. [Running the Complete Dining Philosophers Program](#running-the-complete-dining-philosophers-program)
12. [Performance Considerations and Best Practices](#performance-considerations-and-best-practices)

## Understanding Concurrency in the Dining Philosophers Problem

Before diving into the implementation, let's understand why the Dining Philosophers problem is particularly challenging and why we need sophisticated concurrency tools.

### The Challenge

The Dining Philosophers problem involves five philosophers sitting around a table with five forks. Each philosopher needs two forks to eat, but there are only five forks total. This creates several concurrency challenges:

1. **Resource Contention**: Multiple philosophers may want the same fork simultaneously
2. **Deadlock**: If each philosopher picks up their left fork and waits for their right fork, deadlock occurs
3. **Starvation**: Some philosophers might never get to eat if the scheduling is unfair
4. **Race Conditions**: Without proper synchronization, the state of forks can become corrupted

### Traditional Approaches and Their Problems

**Naive Locking**: Simply using mutexes for each fork can lead to deadlock:
```haskell
-- DEADLOCK PRONE - DON'T DO THIS
philosopher i = do
    takeMVar leftFork
    takeMVar rightFork  -- Deadlock if all philosophers take left fork first
    eat
    putMVar leftFork ()
    putMVar rightFork ()
```

**Lock Ordering**: Always acquire locks in a specific order prevents deadlock but can be complex:
```haskell
-- Prevents deadlock but complex to implement correctly
philosopher i = do
    let (first, second) = if leftForkId < rightForkId 
                         then (leftFork, rightFork) 
                         else (rightFork, leftFork)
    takeMVar first
    takeMVar second
    eat
    putMVar first ()
    putMVar second ()
```

### Why STM is Perfect for This Problem

Software Transactional Memory provides an elegant solution by allowing us to:
- **Atomically acquire multiple resources** without deadlock
- **Automatically retry** when resources aren't available
- **Compose transactions** safely
- **Avoid explicit lock management**

## Introduction to Software Transactional Memory (STM)

Software Transactional Memory is a concurrency control mechanism that provides the illusion of atomic, isolated transactions for memory operations. Think of it like database transactions, but for memory.

### Core STM Concepts

#### 1. Atomicity
All operations in a transaction either complete successfully or none of them do:

```haskell
-- Either both philosophers get forks, or neither does
atomically $ do
    fork1 <- takeTMVar leftFork
    fork2 <- takeTMVar rightFork
    return (fork1, fork2)
```

#### 2. Consistency
Transactions see a consistent view of memory. No partial updates from other transactions are visible:

```haskell
-- This transaction always sees a consistent state
atomically $ do
    balance1 <- readTVar account1
    balance2 <- readTVar account2
    -- balance1 and balance2 are from the same consistent snapshot
    return (balance1 + balance2)
```

#### 3. Isolation
Transactions appear to execute in isolation, even when running concurrently:

```haskell
-- These two transactions appear to run in isolation
-- Transaction 1
atomically $ do
    writeTVar counter 42
    writeTVar flag True

-- Transaction 2  
atomically $ do
    f <- readTVar flag
    c <- readTVar counter
    -- If f is True, then c is guaranteed to be 42
```

#### 4. Composability
STM transactions can be safely composed into larger transactions:

```haskell
withdraw :: TVar Int -> Int -> STM ()
withdraw account amount = do
    balance <- readTVar account
    if balance >= amount
        then writeTVar account (balance - amount)
        else retry

deposit :: TVar Int -> Int -> STM ()
deposit account amount = do
    balance <- readTVar account
    writeTVar account (balance + amount)

-- Compose transactions atomically
atomically $ do
    withdraw account1 100
    deposit account2 100
    -- Either both succeed or both fail
```

### How STM Works Internally

STM uses an optimistic concurrency approach:

1. **Transaction Log**: Each transaction maintains a log of memory reads and writes
2. **Validation**: Before committing, STM validates that all read locations haven't changed
3. **Commit or Retry**: If validation succeeds, changes are committed atomically. If not, the transaction retries
4. **Automatic Retry**: When `retry` is called, the transaction automatically restarts when any read variables change

```haskell
-- Simplified STM execution model
runTransaction :: STM a -> IO a
runTransaction transaction = do
    log <- newTransactionLog
    result <- runWithLog log transaction
    case result of
        Success value -> do
            valid <- validateLog log
            if valid 
                then commitLog log >> return value
                else runTransaction transaction  -- Retry
        Retry -> do
            waitForChanges log
            runTransaction transaction  -- Retry when changes occur
```

### STM vs. Traditional Locking

| Aspect | Traditional Locking | STM |
|--------|-------------------|-----|
| **Deadlock** | Possible, requires careful lock ordering | Impossible, automatic retry |
| **Composability** | Difficult, locks don't compose | Excellent, transactions compose naturally |
| **Error Handling** | Complex, must release locks on exceptions | Automatic, failed transactions rollback |
| **Performance** | Can be faster for simple cases | Better scalability, less contention |
| **Debugging** | Difficult, race conditions are hard to reproduce | Easier, deterministic failure modes |

## Understanding TVars: Transactional Variables

TVars (Transactional Variables) are the building blocks of STM. They're mutable references that can only be safely accessed within STM transactions.

### Basic TVar Operations

#### Creating TVars
```haskell
-- Create a new TVar with an initial value
counter :: IO (TVar Int)
counter = newTVarIO 0

-- Create a TVar within a transaction
counter' :: STM (TVar Int)
counter' = newTVar 0
```

#### Reading TVars
```haskell
-- Read the current value of a TVar
readCounter :: TVar Int -> STM Int
readCounter counter = readTVar counter

-- Example usage
main = do
    counter <- newTVarIO 42
    value <- atomically $ readTVar counter
    print value  -- Prints: 42
```

#### Writing TVars
```haskell
-- Write a new value to a TVar
writeCounter :: TVar Int -> Int -> STM ()
writeCounter counter newValue = writeTVar counter newValue

-- Example usage
main = do
    counter <- newTVarIO 0
    atomically $ writeTVar counter 100
    value <- atomically $ readTVar counter
    print value  -- Prints: 100
```

#### Modifying TVars
```haskell
-- Modify a TVar's value
increment :: TVar Int -> STM ()
increment counter = do
    current <- readTVar counter
    writeTVar counter (current + 1)

-- Or use the convenient modifyTVar
increment' :: TVar Int -> STM ()
increment' counter = modifyTVar counter (+1)

-- Example: Safe concurrent increment
main = do
    counter <- newTVarIO 0
    
    -- Fork 1000 threads that each increment the counter
    replicateM_ 1000 $ forkIO $ 
        atomically $ increment counter
    
    threadDelay 1000000  -- Wait for threads to finish
    
    final <- atomically $ readTVar counter
    print final  -- Will reliably print: 1000
```

### TMVars: Transactional MVars

TMVars are similar to MVars but work within STM transactions. They can be empty or contain a value:

```haskell
-- Create an empty TMVar (like an empty box)
emptyBox :: IO (TMVar String)
emptyBox = newEmptyTMVarIO

-- Create a TMVar with an initial value
fullBox :: IO (TMVar String)
fullBox = newTMVarIO "Hello"

-- Put a value into an empty TMVar
putValue :: TMVar String -> String -> STM ()
putValue box value = putTMVar box value

-- Take a value from a TMVar (blocks if empty)
takeValue :: TMVar String -> STM String
takeValue box = takeTMVar box

-- Example: Producer-Consumer with TMVar
main = do
    mailbox <- newEmptyTMVarIO
    
    -- Producer thread
    forkIO $ do
        atomically $ putTMVar mailbox "Message 1"
        threadDelay 1000000
        atomically $ putTMVar mailbox "Message 2"
    
    -- Consumer thread
    forkIO $ do
        msg1 <- atomically $ takeTMVar mailbox
        putStrLn $ "Received: " ++ msg1
        msg2 <- atomically $ takeTMVar mailbox
        putStrLn $ "Received: " ++ msg2
    
    threadDelay 2000000
```

### Advanced TVar Patterns

#### Conditional Updates
```haskell
-- Only update if condition is met, otherwise retry
updateIf :: TVar Int -> (Int -> Bool) -> (Int -> Int) -> STM ()
updateIf tvar condition update = do
    current <- readTVar tvar
    if condition current
        then writeTVar tvar (update current)
        else retry

-- Example: Only increment if value is less than 100
main = do
    counter <- newTVarIO 95
    
    -- This will succeed
    atomically $ updateIf counter (<100) (+1)
    
    -- This will retry until condition is met (if ever)
    atomically $ updateIf counter (<90) (+1)
```

#### Watching for Changes
```haskell
-- Wait for a TVar to reach a specific value
waitForValue :: Eq a => TVar a -> a -> STM ()
waitForValue tvar expectedValue = do
    current <- readTVar tvar
    if current == expectedValue
        then return ()
        else retry

-- Example: Wait for counter to reach 100
main = do
    counter <- newTVarIO 0
    
    -- Background thread incrementing counter
    forkIO $ replicateM_ 100 $ do
        atomically $ modifyTVar counter (+1)
        threadDelay 10000
    
    -- Wait for completion
    atomically $ waitForValue counter 100
    putStrLn "Counter reached 100!"
```

#### Complex State Updates
```haskell
-- Transfer between accounts atomically
transfer :: TVar Int -> TVar Int -> Int -> STM ()
transfer fromAccount toAccount amount = do
    fromBalance <- readTVar fromAccount
    if fromBalance >= amount
        then do
            writeTVar fromAccount (fromBalance - amount)
            modifyTVar toAccount (+amount)
        else retry  -- Retry when insufficient funds

-- Example: Safe money transfer
main = do
    account1 <- newTVarIO 1000
    account2 <- newTVarIO 500
    
    -- Multiple concurrent transfers
    forkIO $ atomically $ transfer account1 account2 100
    forkIO $ atomically $ transfer account2 account1 50
    forkIO $ atomically $ transfer account1 account2 200
    
    threadDelay 1000000
    
    balance1 <- atomically $ readTVar account1
    balance2 <- atomically $ readTVar account2
    print (balance1, balance2)  -- Total always equals 1500
```

## Project Setup and Dependencies

Let's update our project to include the necessary STM and concurrency dependencies.

### Updating package.yaml

Add the STM-related dependencies to your `package.yaml`:

```yaml
name:                Diner
version:             0.1.0.0
github:              "githubuser/Diner"
license:             BSD-3-Clause
author:              "Author name here"
maintainer:          "example@example.com"
copyright:           "2025 Author name here"

extra-source-files:
- README.md
- CHANGELOG.md

description:         Please see the README on GitHub at <https://github.com/githubuser/Diner#readme>

dependencies:
- base >= 4.7 && < 5
- parsec
- stm >= 2.5          # Software Transactional Memory
- containers >= 0.6   # For Map data structure
- mtl >= 2.2          # For monad transformers (ReaderT)
- random >= 1.2       # For random number generation

ghc-options:
- -Wall
- -Wcompat
- -Widentities
- -Wincomplete-record-updates
- -Wincomplete-uni-patterns
- -Wmissing-export-lists
- -Wmissing-home-modules
- -Wpartial-fields
- -Wredundant-constraints
- -threaded            # Enable threading support

library:
  source-dirs: src

executables:
  Diner-exe:
    main:                Main.hs
    source-dirs:         app
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - Diner

  run-philosophers:
    main:                RunPhilosophers.hs
    source-dirs:         app
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - Diner

tests:
  Diner-test:
    main:                Spec.hs
    source-dirs:         test
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - Diner
    - HUnit >= 1.6
    - QuickCheck >= 2.14
    - test-framework >= 0.8
    - test-framework-hunit >= 0.3
    - test-framework-quickcheck2 >= 0.3
```

Key new dependencies explained:

1. **stm**: The Software Transactional Memory library
2. **containers**: For `Map` data structures to store variables and resources
3. **mtl**: For monad transformers, specifically `ReaderT` for shared state
4. **random**: For random number generation in `rand` expressions
5. **-threaded**: GHC option to enable threading support

## Defining Runtime Values and State

Now let's create `src/Semantics.hs` and define the runtime representation of our language.

### Runtime Values

First, we define what values can exist at runtime:

```haskell
{-# LANGUAGE ScopedTypeVariables #-}

module Semantics where

import Parser
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Exception (catch, SomeException)
import Data.Map (Map)
import qualified Data.Map as Map
import System.Random
import Text.Read (readMaybe)

-- Runtime values
data Value
    = VInt Int
    | VString String
    | VResource (TMVar ())  -- STM transactional variable for resource locking
    deriving (Eq)

instance Show Value where
    show (VInt n) = show n
    show (VString s) = s
    show (VResource _) = "<resource>"
```

Let's understand each runtime value:

#### VInt and VString
These represent basic data types - integers and strings. They're straightforward and correspond directly to literal values in our language.

#### VResource: The Key to Concurrency
```haskell
VResource (TMVar ())
```

This is where the magic happens. Each resource (like a fork) is represented by a `TMVar ()`. Let's break this down:

- **TMVar**: A transactional variable that can be either empty or contain a value
- **()**: The unit type - we don't care about the value, just whether the TMVar is empty or full
- **Empty TMVar = Available Resource**: When the TMVar is empty, the resource is available
- **Full TMVar = Locked Resource**: When the TMVar contains `()`, the resource is locked

This design provides several benefits:

1. **Atomic Acquisition**: Taking from a TMVar automatically makes it unavailable to others
2. **Blocking Behavior**: If the TMVar is empty, `takeTMVar` will block until it becomes available
3. **Composable Locking**: Multiple TMVars can be taken atomically
4. **Automatic Retry**: STM automatically retries when resources become available

### Process Environment and Global State

```haskell
-- Process state containing variable bindings
type Environment = Map String Value

-- ANSI color codes for different threads
data ThreadColor = Red | Green | Yellow | Blue | Magenta | Cyan | White
    deriving (Eq, Show, Enum, Bounded)

colorCode :: ThreadColor -> String
colorCode Red     = "\ESC[31m"
colorCode Green   = "\ESC[32m"
colorCode Yellow  = "\ESC[33m"
colorCode Blue    = "\ESC[34m"
colorCode Magenta = "\ESC[35m"
colorCode Cyan    = "\ESC[36m"
colorCode White   = "\ESC[37m"

resetCode :: String
resetCode = "\ESC[0m"

-- Global state containing all resources and processes - now shared via TVar
data GlobalState = GlobalState
    { globalResources :: Map String (TMVar ())
    , processCounter :: Int
    , threadColors :: Map ThreadId ThreadColor
    } deriving (Eq)

-- Shared global state
type SharedGlobalState = TVar GlobalState
```

#### Environment
Each process (thread) has its own `Environment` - a mapping from variable names to values. This includes:
- Local variables created with `let`
- Resource references bound by `lockAll`
- Loop variables in `foreach`

#### GlobalState
The `GlobalState` contains information shared across all processes:

1. **globalResources**: A map from resource names to their TMVars
2. **processCounter**: For generating unique process IDs
3. **threadColors**: Maps thread IDs to colors for output visualization

#### SharedGlobalState: TVar GlobalState
This is crucial - we wrap the entire global state in a `TVar`. This means:
- **Atomic Updates**: Changes to global state happen atomically
- **Consistent Reads**: Reading global state always sees a consistent snapshot
- **Safe Sharing**: Multiple threads can safely read/modify global state

### Why Use TVar for Global State?

Consider what happens when multiple threads want to declare resources simultaneously:

```haskell
-- WITHOUT TVar (UNSAFE):
declareResource1 = do
    currentState <- readIORef globalStateRef
    let newState = currentState { globalResources = Map.insert "fork1" tmvar1 (globalResources currentState) }
    writeIORef globalStateRef newState

declareResource2 = do
    currentState <- readIORef globalStateRef  -- Might read stale state!
    let newState = currentState { globalResources = Map.insert "fork2" tmvar2 (globalResources currentState) }
    writeIORef globalStateRef newState  -- Might overwrite fork1!

-- WITH TVar (SAFE):
declareResource name tmvar = atomically $ do
    currentState <- readTVar globalStateRef
    let newState = currentState { globalResources = Map.insert name tmvar (globalResources currentState) }
    writeTVar globalStateRef newState
```

With `TVar`, both resource declarations will be applied atomically, preventing lost updates.

## Building the Evaluation Monad

We need a monad that can handle both STM operations and IO operations while sharing global state.

```haskell
-- Initialize empty global state
initialState :: GlobalState
initialState = GlobalState Map.empty 0 Map.empty

-- Evaluation monad that uses shared global state
type EvalM = ReaderT SharedGlobalState IO

-- Run the evaluator with shared state
runEval :: SharedGlobalState -> EvalM a -> IO a
runEval sharedState action = runReaderT action sharedState
```

### Understanding the EvalM Monad

Our evaluation monad `EvalM` is built using `ReaderT`:

```haskell
type EvalM = ReaderT SharedGlobalState IO
```

This gives us:

#### 1. IO Capabilities
We can perform IO operations like printing, threading, and delays:
```haskell
colorPrint :: String -> EvalM ()
colorPrint msg = liftIO $ putStrLn msg
```

#### 2. Shared State Access
We can access the shared global state from anywhere:
```haskell
getGlobalState :: EvalM GlobalState
getGlobalState = do
    sharedState <- ask  -- Get the TVar from ReaderT
    liftIO $ readTVarIO sharedState  -- Read current value
```

#### 3. STM Integration
We can run STM transactions within our monad:
```haskell
updateGlobalState :: (GlobalState -> GlobalState) -> EvalM ()
updateGlobalState f = do
    sharedState <- ask
    liftIO $ atomically $ modifyTVar sharedState f
```

### Helper Functions for State Management

```haskell
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
```

#### updateGlobalState Explained

```haskell
updateGlobalState :: (GlobalState -> GlobalState) -> EvalM ()
updateGlobalState f = do
    sharedState <- ask                    -- Get the TVar GlobalState
    liftIO $ atomically $ modifyTVar sharedState f  -- Atomically apply function f
```

This function:
1. Gets the shared state `TVar` from the `ReaderT` environment
2. Atomically applies the update function `f` to the global state
3. The `modifyTVar` ensures the update happens atomically

Example usage:
```haskell
-- Add a new resource to global state
addResource :: String -> TMVar () -> EvalM ()
addResource name resource = 
    updateGlobalState $ \s -> s { 
        globalResources = Map.insert name resource (globalResources s) 
    }
```

#### lookupGlobalResource Explained

```haskell
lookupGlobalResource :: String -> EvalM (Maybe (TMVar ()))
lookupGlobalResource name = do
    sharedState <- ask                           -- Get TVar
    globalState <- liftIO $ readTVarIO sharedState  -- Read current state
    return $ Map.lookup name (globalResources globalState)  -- Find resource
```

Note: We use `readTVarIO` instead of `atomically $ readTVar` because:
- We're only reading, not updating
- We don't need this read to be part of a larger transaction
- It's more efficient for simple reads

### Thread Color Management

To visualize concurrent execution, we assign colors to threads:

```haskell
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
                    (c:_) -> c
                    []    -> availableColors !! (Map.size (threadColors globalState) `mod` length availableColors)
            
            -- Update global state with new color assignment
            liftIO $ atomically $ modifyTVar sharedState $ \s -> 
                s { threadColors = Map.insert tid nextColor (threadColors s) }
            
            return nextColor

-- Colored print function
colorPrint :: String -> EvalM ()
colorPrint msg = do
    tid <- liftIO myThreadId
    color <- getThreadColor
    let tidStr = take 8 $ show tid -- Truncate thread ID for readability
    liftIO $ putStrLn $ colorCode color ++ "[" ++ tidStr ++ "] " ++ msg ++ resetCode
```

This color system helps visualize which philosopher (thread) is performing each action, making it easier to understand the concurrent execution.

## Implementing Expression Evaluation

Now let's implement the evaluation of expressions:

```haskell
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
```

### Key Points in Expression Evaluation

#### Variable Lookup
```haskell
Var name -> 
    case Map.lookup name env of
        Just val -> return val
        Nothing -> -- Error handling
```

Variables are looked up in the local environment. If not found, we return a placeholder value rather than crashing.

#### Random Number Generation
```haskell
Rand e1 e2 -> do
    v1 <- evalExpr env e1
    v2 <- evalExpr env e2
    case (v1, v2) of
        (VInt lower, VInt upper) -> liftIO $ do
            gen <- newStdGen
            let (val, _) = randomR (lower, upper) gen
            return $ VInt val
```

The `rand` expression generates random numbers within the specified range. This is used to simulate variable thinking and eating times for philosophers.

#### Type Safety
Each operation checks that its operands have the correct types. If not, we print an error and return a sensible default value rather than crashing the program.

## Implementing Statement Execution

Statement execution is where the real concurrency magic happens:

```haskell
-- Safe resource lookup in environment
lookupResource :: String -> Environment -> Maybe (TMVar ())
lookupResource name env = case Map.lookup name env of
    Just (VResource tmvar) -> Just tmvar
    _ -> Nothing

-- Statement execution with improved error handling
execStmt :: Environment -> Statement -> EvalM Environment
execStmt env stmt = do
    sharedState <- ask
    result <- liftIO $ (runEval sharedState $ execStmt' env stmt) `catch` \(e :: SomeException) -> do
        putStrLn $ "Runtime error: " ++ show e
        return env
    return result

execStmt' :: Environment -> Statement -> EvalM Environment
execStmt' environment statement = case statement of
        -- ... we'll implement each statement type
```

Let's implement each statement type, focusing on the STM aspects:

### Think Statement
```haskell
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
```

The `think` statement simulates a philosopher thinking. It's straightforward - evaluate the duration expression and delay for that amount of time.

### Eat Statement
```haskell
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
