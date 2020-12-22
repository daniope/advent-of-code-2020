module Main where

import Data.Char
import Text.Parsec
import Text.Parsec.String
import System.Environment

type Part = Int
type Form = String
type Group = [Form]
type Groups = [Group]

form :: Parser Form
form = many1 letter

group :: Parser Group
group = endBy1 form endOfLine

groups :: Parser Groups
groups = sepBy group newline

answer :: Char -> Group -> Part -> Bool
answer c [] 1     = False
answer c [] 2     = True
answer c (f:fs) 1 = elem c f || answer c fs 1
answer c (f:fs) 2 = elem c f && answer c fs 2

sheet :: Group -> Part -> [Char]
sheet g p = [ x | x <- ['a'..'z'], answer x g p]

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
