module Main (main) where

import System.Environment (getArgs, withArgs)
import Test.Framework (defaultMain)
import qualified Parser.ParserSpec as P

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> defaultMain allTests
        _  -> withArgs args $ defaultMain allTests

-- Gather all tests from different test modules
allTests = P.tests
