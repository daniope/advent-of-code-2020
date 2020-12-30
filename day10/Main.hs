module Main where

import System.Environment

parse :: FilePath -> IO [Int]
parse = fmap rd . (fmap lines . readFile)
    where rd = map read

main :: IO ()
main = do
    { args <- getArgs
    ; js <- parse $ head args
    ; putStrLn $ "Part 1: " ++ show js
    }
