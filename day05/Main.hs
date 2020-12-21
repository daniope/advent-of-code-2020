module Main where

import System.Environment

readPasses :: FilePath -> IO [String]
readPasses = fmap lines . readFile

main :: IO ()
main = do
    { args <- getArgs
    ; passes <- readPasses $ head args
    ; print passes
    }
