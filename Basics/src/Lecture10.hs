module Lecture10 where 


import Control.Concurrent 
import Control.Concurrent.STM
import Control.Monad (replicateM, forever, replicateM_, forM_)
import System.Random
-- Represent a fork as an MVar
type Fork = MVar ()
-- Fork contains value ()
-- MVar a = IsAvailable a | NotAvailable 
--
codeExample :: IO ()
codeExample = do
  a <- newMVar 5 
  value <- takeMVar a 
  print value 
  putMVar a 10 
  value <- takeMVar a 
  print value 
  pure () 

randomDelay :: IO ()
randomDelay = do
        delay <- randomRIO (100000, 500000)
        threadDelay delay


-- Create forks
createForks :: Int -> IO [Fork]
createForks n = replicateM n (newMVar ())

-- Try to eat (DEADLOCK PRONE!)
philosopher :: Int -> Fork -> Fork -> IO ()
philosopher id leftFork rightFork = replicateM_ 10000 $ do
    putStrLn $ "Philosopher " ++ show id ++ " thinking"
    randomDelay
    putStrLn $ "Philosopher " ++ show id ++ " hungry"
    takeMVar leftFork   -- Get left fork
    randomDelay
    takeMVar rightFork  -- Get right fork (DEADLOCK!)
    randomDelay   
    putStrLn $ "Philosopher " ++ show id ++ " eating"
    
    putMVar leftFork ()
    randomDelay
    putMVar rightFork ()

testDeadlock :: IO ()
testDeadlock = do
    let n = 2
    forks <- createForks n
  
    -- Start all philosophers
    mapM_ (\i -> do
        let leftFork = forks !! i
            rightFork = forks !! ((i + 1) `mod` n)
        forkIO $ philosopher i leftFork rightFork
      ) [0..n-1]
        
    threadDelay 500000000  -- Wait 5 seconds



mVarDeadlock :: IO ()
mVarDeadlock = do
    putStrLn "=== Deadlock Version ==="
    counter <- newMVar 0
    counter2 <- newMVar 0 
    
    -- Spawn 100 threads with inconsistent lock ordering
    forM_ [1..100] $ \i -> forkIO $ do
        if even i
        then do
            -- Even threads: take counter first, then counter2
            current <- takeMVar counter
            threadDelay 1000  -- Increase chance of deadlock
            current2 <- takeMVar counter2
            putMVar counter (current2 + 1)
            putMVar counter2 (current + 1)
        else do
            -- Odd threads: take counter2 first, then counter
            current2 <- takeMVar counter2  
            threadDelay 1000  -- Increase chance of deadlock
            current <- takeMVar counter
            putMVar counter2 (current + 1) 
            putMVar counter (current2 + 1)
    

    threadDelay 5000000

    c <- takeMVar counter 
    c2 <- takeMVar counter2 
    putStrLn $ show c ++ show c2
    putMVar counter c 
    putMVar counter2 c2

counter :: IO ()
counter = do
    counter <- newTVarIO 0
    
    -- Spawn 100 threads that increment the counter
    replicateM_ 100 $ forkIO $ 
        atomically $ do
            current <- readTVar counter
            writeTVar counter (current + 1)
    
    threadDelay 1000000  -- Wait for completion
    
    final <- readTVarIO counter
    putStrLn $ "Final count: " ++ show final 



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
runBalance :: IO ()
runBalance = do
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
