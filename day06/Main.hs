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

answer :: Char -> Group -> Int -> Bool
answer c [] 1     = False
answer c [] 2     = True
answer c (f:fs) 1 = elem c f || answer c fs 1
answer c (f:fs) 2 = elem c f && answer c fs 2

sheet :: Group -> Int -> [Char]
sheet g part = [ x | x <- ['a'..'z'], answer x g part]

part1 :: Groups -> Int
part1 []     = 0
part1 (g:gs) = (length $ sheet g 1) + part1 gs 

part2 :: Groups -> Int
part2 []     = 0
part2 (g:gs) = (length $ sheet g 2) + part2 gs 

main :: IO ()
main = do
    { args <- getArgs
    ; input <- parseFromFile groups $ head args
    ; case input of
        Left err  -> print err
        Right gs  -> do
            { putStrLn $ "Part 1: " ++ show (part1 gs)
            ; putStrLn $ "Part 2: " ++ show (part2 gs)
            }
    }
