module Main where

import Data.Char
import Text.Parsec.Char
import Text.Parsec.String
import Text.ParserCombinators.Parsec
import System.Environment

data Policy = Policy {
      min :: Int
    , max :: Int
    , letter :: Char
    , password :: String
    } deriving (Eq, Show) 

main :: IO ()
main = do 
    { args <- getArgs
    ; result <- parseFromFile anyChar $ head args
    ; case result of
        Left err  -> print err
        Right xs  -> print xs
    }
