module Main where

import Data.Char
import Text.Parsec
import Text.Parsec.String
import System.Environment

type Form = String
type Group = [Form]
type Groups = [Group]

form :: Parser Form
form = many1 letter

group :: Parser Group
group = endBy1 form endOfLine

groups :: Parser Groups
groups = sepBy group newline

main :: IO ()
main = do
    { args <- getArgs
    ; groups <- parseFromFile groups $ head args
    ; case groups of
        Left err  -> print err
        Right gs  -> do
            { putStrLn $ "Part 1: " ++ show gs
            }
    }
