module Lecture10 where 


import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (replicateM, forever, replicateM_)

-- Represent a fork as an MVar
type Fork = MVar ()

-- Create forks
createForks :: Int -> IO [Fork]
createForks n = replicateM n (newMVar ())

-- Try to eat (DEADLOCK PRONE!)
philosopher :: Int -> Fork -> Fork -> IO ()
philosopher id leftFork rightFork = replicateM_ 10000 $ do
    putStrLn $ "Philosopher " ++ show id ++ " thinking"
    -- threadDelay 10000
    
    putStrLn $ "Philosopher " ++ show id ++ " hungry"
    takeMVar leftFork   -- Get left fork
    takeMVar rightFork  -- Get right fork (DEADLOCK!)
    
    putStrLn $ "Philosopher " ++ show id ++ " eating"
    -- threadDelay 50000
    
    putMVar leftFork ()
    putMVar rightFork ()

testDeadlock :: IO ()
testDeadlock = do
    let n = 10
    forks <- createForks n
  
    -- Start all philosophers
    mapM_ (\i -> do
        let leftFork = forks !! i
            rightFork = forks !! ((i + 1) `mod` n)
        forkIO $ philosopher i leftFork rightFork
      ) [0..n-1]
        
    threadDelay 50000000  -- Wait 5 seconds
    putStrLn "If you see this, no deadlock occurred"


