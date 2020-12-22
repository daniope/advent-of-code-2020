module Main where

import Data.List
import Text.Parsec
import Text.Parsec.String
import System.Environment

type Part = Int
type Form = String
type Group = [Form]
type Groups = [Group]

form :: Parser Form
form = many1 letter

groups :: Parser Groups
groups = sepBy (endBy1 form endOfLine) newline

sheet :: Group -> Part -> [Char]
sheet (f:[]) _ = f
sheet (f:fs) 1 = union f $ sheet fs 1
sheet (f:fs) 2 = intersect f $ sheet fs 2

solve :: Groups -> Part -> Int
solve [] p     = 0
solve (g:gs) p = (length $ sheet g p) + solve gs p

main :: IO ()
main = do
    { args <- getArgs
    ; input <- parseFromFile groups $ head args
    ; case input of
        Left err  -> print err
        Right gs  -> do
            { putStrLn $ "Part 1: " ++ show (solve gs 1)
            ; putStrLn $ "Part 2: " ++ show (solve gs 2)
            }
    }
