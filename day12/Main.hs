module Main where

import Text.Parsec
import Text.Parsec.String
import System.Environment

data Action
    = N -- North 
    | E -- East
    | S -- South
    | W -- West
    | F -- Forward
    | R -- Right
    | L -- Left
    deriving (Eq, Show)

type Instruction = (Action, Int)

data Direction
    = North
    | South
    | East
    | West
    deriving (Show)

type Position =((Direction, Int), (Direction, Int))

data Ship = Ship
    { position :: Position
    , path     :: [Position]
    } deriving (Show)

toAction :: Char -> Action
toAction c = case c of
    'N' -> N 
    'E' -> E 
    'S' -> S 
    'W' -> W 
    'F' -> F
    'R' -> R 
    'L' -> L 
    otherwise -> error "Invalid direction"

integer :: Parser Int
integer = fmap read $ many1 digit

instruction :: Parser Instruction
instruction = do
    { d <- letter
    ; n <- integer
    ; return (toAction d, n)
    }

instructions :: Parser [Instruction]
instructions = endBy1 instruction endOfLine

main :: IO ()
main = do
    { args <- getArgs
    ; content <- parseFromFile instructions $ head args
    ; case content of
        Left err -> print err
        Right cs -> putStrLn $ "Part 1: " ++ show cs
    }
