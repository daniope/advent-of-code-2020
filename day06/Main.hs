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

answer :: Char -> Group -> Bool
answer c []     = False
answer c (f:fs) = elem c f || answer c fs 

sheet :: Group -> [Char]
sheet g = [ x | x <- ['a'..'z'], answer x g ]

part1 :: Groups -> Int
part1 []     = 0
part1 (g:gs) = (length $ sheet g) + part1 gs 

main :: IO ()
main = do
    { args <- getArgs
    ; input <- parseFromFile groups $ head args
    ; case input of
        Left err  -> print err
        Right gs  -> putStrLn $ "Part 1: " ++ show (part1 gs)
    }
