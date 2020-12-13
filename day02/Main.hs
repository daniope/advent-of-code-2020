module Main where

import Data.Char
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import System.Environment

data Policy = Policy {
      min :: Int
    , max :: Int
    , character :: Char
    , password :: String
    } deriving (Eq, Show) 

integer :: Parser Int
integer = fmap read $ many1 digit

policy :: Parser Policy
policy = do 
    { min       <- integer
    ; _         <- oneOf "-"
    ; max       <- integer
    ; _         <- space
    ; character <- anyChar
    ; _         <- oneOf ":"
    ; _         <- space
    ; password  <- many1 letter
    ; _         <- endOfLine
    ; return (Policy min max character password)
    }

policies :: Parser [Policy]
policies = many1 policy

is_valid :: Policy -> Bool
is_valid (Policy min max character password) = do
    { let occ = length $ filter (== character) password
    ; (occ >= min) && (occ <= max)
    }

main :: IO ()
main = do 
    { args <- getArgs
    ; result <- parseFromFile policies $ head args
    ; case result of
        Left err  -> print err
        Right xs  -> print $ map is_valid xs
    }
