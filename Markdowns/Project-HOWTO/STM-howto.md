# Software Transactional Memory (STM) 

This tutorial introduces Software Transactional Memory (STM) in the context of our Dining Philosophers parser project. We'll start with the basics and build up to implementing the concurrency features needed for our language evaluator.

## What We're Building Towards

In our Dining Philosophers language, we have statements like:
```
declareResource "fork1";
lockAll ["fork1", "fork2"] [left, right];
eat 300 resource left resource right;
unlockAll [left, right];
```

These require sophisticated concurrency control that STM makes elegant and safe.

## Part 1: Why Do We Need STM?

### The Problem: Simulating Concurrent Philosophers

Our parser creates AST nodes like:
```haskell
data Statement = 
    -- ... other statements
    | LockAll [Expr] [String]      -- Lock multiple resources atomically
    | UnlockAll [Resource]         -- Release multiple resources
    | Spawn Expr [Statement]       -- Create concurrent processes
```

When we evaluate these, we need:
- **Multiple processes** (philosophers) running concurrently
- **Shared resources** (forks) that can be locked/unlocked
- **Deadlock prevention** when multiple processes want the same resources
- **Atomic operations** to prevent race conditions

Traditional approaches fail here. Let's see why.

### Traditional Approach: MVars (Don't Do This!)

```haskell
-- Let's create a simple MVar-based approach (PROBLEMATIC!)
module BadConcurrency where

import Control.Concurrent
import Control.Monad (replicateM, forever)

-- Represent a fork as an MVar
type Fork = MVar ()

-- Create forks
createForks :: Int -> IO [Fork]
createForks n = replicateM n (newMVar ())

-- Try to eat (DEADLOCK PRONE!)
philosopher :: Int -> Fork -> Fork -> IO ()
philosopher id leftFork rightFork = forever $ do
    putStrLn $ "Philosopher " ++ show id ++ " thinking"
    
    putStrLn $ "Philosopher " ++ show id ++ " hungry"
    takeMVar leftFork   -- Get left fork
    takeMVar rightFork  -- Get right fork (DEADLOCK!)
    
    putStrLn $ "Philosopher " ++ show id ++ " eating"
    
    putMVar leftFork ()
    putMVar rightFork ()
```

**Live Coding Exercise 1**: Let's implement this and see the deadlock!

```haskell
-- In GHCi or a test file
testDeadlock :: IO ()
testDeadlock = do
    forks <- createForks 3
    let fork1 = forks !! 0
        fork2 = forks !! 1  
        fork3 = forks !! 2
    
    -- Start philosophers (deadlock prone!)
    forkIO $ philosopher 1 fork1 fork2
    forkIO $ philosopher 2 fork2 fork3  
    forkIO $ philosopher 3 fork3 fork1
    
    threadDelay 5000000  
    putStrLn "If you see this, no deadlock occurred"
```

**What happens**: All philosophers grab their left fork, then wait forever for their right fork.

## Part 2: Enter STM - The Solution

### Core STM Concepts

STM provides **transactions** - blocks of code that execute atomically:

```haskell
import Control.Concurrent.STM

-- STM transactions are like database transactions
atomically $ do
    -- Either all of these operations succeed,
    -- or none of them do
    balance1 <- readTVar account1  
    balance2 <- readTVar account2
    writeTVar account1 (balance1 - 100)
    writeTVar account2 (balance2 + 100)
```

**Live Coding Exercise 2**: Let's create our first STM transaction

```haskell
-- Simple counter with STM
module STMBasics where

import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad

-- Create a shared counter
main1 :: IO ()
main1 = do
    counter <- newTVarIO 0
    
    -- Spawn 100 threads that increment the counter
    replicateM_ 100 $ forkIO $ 
        atomically $ do
            current <- readTVar counter
            writeTVar counter (current + 1)
    
    threadDelay 1000000  -- Wait for completion
    
    final <- readTVarIO counter
    putStrLn $ "Final count: " ++ show final  -- Always 100!
```

**Key insight**: With MVars, this would often give wrong results due to race conditions. With STM, it's always correct.

## Part 3: TVars - The Building Blocks

### TVar Operations

```haskell
-- Creating TVars
newTVar :: a -> STM (TVar a)           -- Create in transaction
newTVarIO :: a -> IO (TVar a)          -- Create in IO

-- Reading and writing
readTVar :: TVar a -> STM a            -- Read current value
writeTVar :: TVar a -> a -> STM ()     -- Write new value
modifyTVar :: TVar a -> (a -> a) -> STM ()  -- Modify with function

-- Running transactions
atomically :: STM a -> IO a            -- Execute transaction
```

**Live Coding Exercise 3**: Bank account transfers

```haskell
-- Bank account example
type Account = TVar Int

transfer :: Account -> Account -> Int -> STM ()
transfer from to amount = do
    fromBalance <- readTVar from
    toBalance <- readTVar to
    
    -- Check if transfer is valid
    if fromBalance >= amount
        then do
            writeTVar from (fromBalance - amount)
            writeTVar to (toBalance + amount)
        else retry  -- Retry when insufficient funds

-- Test it
main2 :: IO ()
main2 = do
    account1 <- newTVarIO 1000
    account2 <- newTVarIO 500
    
    putStrLn "Initial balances:"
    bal1 <- readTVarIO account1
    bal2 <- readTVarIO account2
    putStrLn $ "Account 1: " ++ show bal1
    putStrLn $ "Account 2: " ++ show bal2
    
    -- Transfer money
    atomically $ transfer account1 account2 300
    
    putStrLn "After transfer:"
    bal1' <- readTVarIO account1
    bal2' <- readTVarIO account2
    putStrLn $ "Account 1: " ++ show bal1'
    putStrLn $ "Account 2: " ++ show bal2'
```

## Part 4: TMVars - For Resource Management

TMVars are perfect for resources that can be "taken" and "put back":

```haskell
-- TMVar operations
newTMVar :: a -> STM (TMVar a)         -- Create full TMVar
newEmptyTMVar :: STM (TMVar a)         -- Create empty TMVar
newTMVarIO :: a -> IO (TMVar a)        -- Create full in IO
newEmptyTMVarIO :: IO (TMVar a)        -- Create empty in IO

takeTMVar :: TMVar a -> STM a          -- Take (blocks if empty)
putTMVar :: TMVar a -> a -> STM ()     -- Put (blocks if full)
readTMVar :: TMVar a -> STM a          -- Read without taking
tryTakeTMVar :: TMVar a -> STM (Maybe a)  -- Non-blocking take
```

**Live Coding Exercise 4**: Resource as TMVar

```haskell
-- A resource that can be acquired and released
type Resource = TMVar ()

-- Create a resource (initially available)
createResource :: IO Resource
createResource = newTMVarIO ()

-- Acquire resource (take the () out)
acquireResource :: Resource -> STM ()
acquireResource resource = do
    () <- takeTMVar resource  -- Blocks if already taken
    return ()

-- Release resource (put () back)
releaseResource :: Resource -> STM ()
releaseResource resource = putTMVar resource ()

-- Test resource sharing
main3 :: IO ()
main3 = do
    resource <- createResource
    
    -- Process 1
    forkIO $ do
        putStrLn "Process 1: Trying to acquire resource"
        atomically $ acquireResource resource
        putStrLn "Process 1: Got resource, working..."
        threadDelay 2000000  -- Work for 2 seconds
        putStrLn "Process 1: Releasing resource"
        atomically $ releaseResource resource
    
    -- Process 2 (will wait)
    forkIO $ do
        threadDelay 500000  -- Start after process 1
        putStrLn "Process 2: Trying to acquire resource"
        atomically $ acquireResource resource
        putStrLn "Process 2: Got resource, working..."
        threadDelay 1000000
        putStrLn "Process 2: Releasing resource"
        atomically $ releaseResource resource
    
    threadDelay 5000000
```

## Part 5: The Magic of Atomic Composition

The killer feature of STM: transactions compose!

**Live Coding Exercise 5**: Acquiring multiple resources atomically

```haskell
-- Acquire multiple resources atomically (NO DEADLOCK!)
acquireResources :: [Resource] -> STM ()
acquireResources resources = mapM_ acquireResource resources

-- Release multiple resources atomically  
releaseResources :: [Resource] -> STM ()
releaseResources resources = mapM_ releaseResource resources

-- Dining philosopher using STM
stmPhilosopher :: Int -> Resource -> Resource -> IO ()
stmPhilosopher id leftFork rightFork = forever $ do
    putStrLn $ "Philosopher " ++ show id ++ " thinking"
    threadDelay 1000000
    
    putStrLn $ "Philosopher " ++ show id ++ " hungry"
    -- Atomically acquire BOTH forks (no deadlock!)
    atomically $ do
        acquireResource leftFork
        acquireResource rightFork
    
    putStrLn $ "Philosopher " ++ show id ++ " eating"
    threadDelay 500000
    
    -- Atomically release both forks
    atomically $ do
        releaseResource leftFork
        releaseResource rightFork

-- Test with 3 philosophers (no deadlock!)
main4 :: IO ()
main4 = do
    forks <- replicateM 3 createResource
    let fork1 = forks !! 0
        fork2 = forks !! 1
        fork3 = forks !! 2
    
    putStrLn "Starting philosophers (STM version - no deadlock!)"
    forkIO $ stmPhilosopher 1 fork1 fork2
    forkIO $ stmPhilosopher 2 fork2 fork3
    forkIO $ stmPhilosopher 3 fork3 fork1
    
    threadDelay 10000000  -- Run for 10 seconds
```

**Key insight**: The atomic acquisition of both forks prevents deadlock. If any fork is unavailable, the entire transaction retries.

## Part 6: Connecting to Our Parser

Now let's connect this to our Dining Philosophers language:

```haskell
-- Our AST from the parser
data Statement = 
    DeclareResource Expr           -- declareResource "fork1"
    | LockAll [Expr] [String]      -- lockAll ["fork1", "fork2"] [left, right]  
    | UnlockAll [Resource]         -- unlockAll [left, right]
    | Spawn Expr [Statement]       -- spawn "philosopher1" { ... }
    -- ... other statements
```

**Live Coding Exercise 6**: Basic evaluator structure

```haskell
-- Runtime values
data Value = 
    VInt Int
    | VString String  
    | VResource (TMVar ())  -- This is our STM resource!

-- Environment for variable bindings
type Environment = Map String Value

-- Global state with all declared resources
data GlobalState = GlobalState
    { globalResources :: Map String (TMVar ())  -- name -> resource
    }

type SharedGlobalState = TVar GlobalState

-- Evaluation monad
type EvalM = ReaderT SharedGlobalState IO

-- Initialize empty state
initialState :: GlobalState
initialState = GlobalState Map.empty

-- Helper to update global state atomically
updateGlobalState :: (GlobalState -> GlobalState) -> EvalM ()
updateGlobalState f = do
    sharedState <- ask
    liftIO $ atomically $ modifyTVar sharedState f
```

**Live Coding Exercise 7**: Implementing DeclareResource

```haskell
-- Evaluate a DeclareResource statement
execDeclareResource :: Expr -> EvalM ()
execDeclareResource expr = do
    -- For now, assume expr evaluates to a string
    let resourceName = "fork1"  -- We'll implement expression evaluation later
    
    -- Create new TMVar resource
    resource <- liftIO $ newTMVarIO ()
    
    -- Add to global state atomically
    updateGlobalState $ \state -> state {
        globalResources = Map.insert resourceName resource (globalResources state)
    }
    
    liftIO $ putStrLn $ "Declared resource: " ++ resourceName
```

**Live Coding Exercise 8**: Implementing LockAll

```haskell
-- Look up a resource from global state
lookupGlobalResource :: String -> EvalM (Maybe (TMVar ()))
lookupGlobalResource name = do
    sharedState <- ask
    globalState <- liftIO $ readTVarIO sharedState
    return $ Map.lookup name (globalResources globalState)

-- Execute LockAll statement
execLockAll :: [String] -> [String] -> Environment -> EvalM Environment
execLockAll resourceNames varNames env = do
    -- Look up all resources
    maybeResources <- mapM lookupGlobalResource resourceNames
    
    case sequence maybeResources of
        Just resources -> do
            -- Atomically acquire all resources
            liftIO $ atomically $ mapM_ takeTMVar resources
            
            -- Bind resources to variable names in environment
            let resourceValues = map VResource resources
                newBindings = Map.fromList $ zip varNames resourceValues
                newEnv = Map.union newBindings env
            
            liftIO $ putStrLn $ "Locked resources: " ++ show resourceNames
            return newEnv
            
        Nothing -> do
            liftIO $ putStrLn "Error: Some resources not found"
            return env
```

## Part 7: Why This Works (The Theory)

### Optimistic Concurrency

STM uses **optimistic concurrency control**:

1. **Read Phase**: Transaction reads values, keeping a log
2. **Validation**: Before committing, check if any read values changed  
3. **Commit**: If validation passes, apply all writes atomically
4. **Retry**: If validation fails, restart the transaction

```haskell
-- Conceptual STM implementation
runTransaction :: STM a -> IO a
runTransaction transaction = do
    log <- newLog
    result <- runWithLog log transaction
    case result of
        Success value -> do
            valid <- validateLog log  -- Check if reads are still valid
            if valid 
                then commitLog log >> return value  -- Commit atomically
                else runTransaction transaction     -- Retry
        Retry -> do
            waitForChanges log        -- Wait for any read variable to change
            runTransaction transaction -- Retry
```

### Composability

The key insight: **transactions compose**!

```haskell
-- These are both valid transactions
transaction1 :: STM ()
transaction1 = do
    acquireResource fork1
    acquireResource fork2

transaction2 :: STM ()  
transaction2 = do
    transaction1  -- Reuse transaction1
    updateStats   -- Add more operations
```

This is **impossible** with traditional locking - you can't safely compose lock acquisitions.

## Part 8: Live Coding Complete Example

Let's build a minimal working example:

**Live Coding Exercise 9**: End-to-end STM example

```haskell
module STMExample where

import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map

-- Simple version of our language values
data Value = VResource (TMVar ())

-- Simple environment  
type Env = Map String Value

-- Simple global state
type GlobalState = Map String (TMVar ())

-- Create resources
declareResource :: String -> TVar GlobalState -> IO ()
declareResource name globalState = do
    resource <- newTMVarIO ()
    atomically $ modifyTVar globalState (Map.insert name resource)
    putStrLn $ "Declared resource: " ++ name

-- Lock resources
lockResources :: [String] -> TVar GlobalState -> IO (Maybe [TMVar ()])
lockResources names globalState = do
    state <- readTVarIO globalState
    let maybeResources = map (`Map.lookup` state) names
    case sequence maybeResources of
        Just resources -> do
            atomically $ mapM_ takeTMVar resources
            putStrLn $ "Locked: " ++ show names
            return (Just resources)
        Nothing -> do
            putStrLn "Some resources not found"
            return Nothing

-- Unlock resources
unlockResources :: [TMVar ()] -> IO ()
unlockResources resources = do
    atomically $ mapM_ (`putTMVar` ()) resources
    putStrLn "Unlocked resources"

-- Philosopher process
philosopher :: Int -> [String] -> TVar GlobalState -> IO ()
philosopher id forkNames globalState = forever $ do
    putStrLn $ "Philosopher " ++ show id ++ " thinking"
    threadDelay 1000000
    
    putStrLn $ "Philosopher " ++ show id ++ " trying to get forks"
    maybeResources <- lockResources forkNames globalState
    
    case maybeResources of
        Just resources -> do
            putStrLn $ "Philosopher " ++ show id ++ " eating"
            threadDelay 800000
            unlockResources resources
            putStrLn $ "Philosopher " ++ show id ++ " done eating"
        Nothing -> 
            putStrLn $ "Philosopher " ++ show id ++ " couldn't get forks"

-- Main demo
main :: IO ()
main = do
    globalState <- newTVarIO Map.empty
    
    -- Declare resources
    declareResource "fork1" globalState
    declareResource "fork2" globalState
    declareResource "fork3" globalState
    
    -- Start philosophers
    forkIO $ philosopher 1 ["fork1", "fork2"] globalState
    forkIO $ philosopher 2 ["fork2", "fork3"] globalState  
    forkIO $ philosopher 3 ["fork3", "fork1"] globalState
    
    putStrLn "Press Enter to stop..."
    getLine
    return ()
```

## Part 9: Integration Strategy

Now we can integrate this into our full parser project:

### Step 1: Add STM to package.yaml
```yaml
dependencies:
- base >= 4.7 && < 5
- parsec
- stm >= 2.5          # Add this
- containers >= 0.6   # Add this for Map
- mtl >= 2.2          # Add this for ReaderT
```

### Step 2: Extend the AST runtime values
```haskell
-- In Parser.hs, we already have the AST
-- In Semantics.hs, add runtime values:
data Value = 
    VInt Int
    | VString String
    | VResource (TMVar ())  -- STM resource
```

### Step 3: Build the evaluator structure
```haskell
-- Environment for each process
type Environment = Map String Value

-- Shared global state
data GlobalState = GlobalState 
    { globalResources :: Map String (TMVar ())
    }

type SharedGlobalState = TVar GlobalState

-- Evaluation monad
type EvalM = ReaderT SharedGlobalState IO
```

### Step 4: Implement statement execution
```haskell
execStatement :: Environment -> Statement -> EvalM Environment
execStatement env stmt = case stmt of
    DeclareResource expr -> -- Use STM to create and register resource
    LockAll exprs vars -> -- Use STM to atomically acquire resources  
    UnlockAll resources -> -- Use STM to atomically release resources
    Spawn expr stmts -> -- Fork new thread with shared global state
    -- ... other statements
```

## Part 10: Next Steps

After this tutorial, you'll be ready to:

1. **Add STM dependencies** to your project
2. **Extend the runtime values** to include STM resources
3. **Implement the core STM operations** for resource management
4. **Build the concurrent evaluator** that can run multiple philosophers
5. **Test deadlock-free execution** of the dining philosophers problem

The beauty of STM is that it makes concurrent programming **composable** and **deadlock-free**. Instead of carefully ordering locks and worrying about exceptions, you just write the logic naturally and STM handles the coordination.

## Key Takeaways

1. **STM eliminates deadlock** through optimistic transactions
2. **TVars and TMVars** provide thread-safe mutable references
3. **Transactions compose naturally** - the killer feature
4. **Resources map perfectly to TMVars** in our dining philosophers problem
5. **The parser AST drives the STM operations** - clean separation of concerns

Ready to integrate STM into the full evaluator? Let's do it!
