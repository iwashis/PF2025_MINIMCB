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

Ready to integrate STM into the full evaluator? Let's do it!
