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

type Angle = Float
type Direction = (Int, Int)
type Location = (Int, Int)

data Ship = Ship
    { angle     :: Float
    , location  :: Location
    , wayPoint  :: Location
    , path      :: [Instruction]
    } deriving (Show)

newShip :: Ship
newShip = Ship 0.0 (0,0) (10,1) []

toAction :: Char -> Action
toAction c = case c of
    'N' -> N 
    'E' -> E 
    'S' -> S 
    'W' -> W 
    'F' -> F
    'R' -> R 
    'L' -> L 
    otherwise -> error "Invalid action"

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

getDirection :: Angle -> Direction
getDirection n = (round $ cos (n * pi / 180.0), round $ sin (n * pi / 180.0))

rotate :: Angle -> Instruction -> Angle
rotate ag (a, n) = ag + i * (fromIntegral n :: Float)
    where i = case a of
            R -> -1
            L -> 1

translate :: Angle -> Location -> Instruction -> Location
translate ag (x, y) (a, n) = (x + i * n, y + j * n)
    where iag = case a of
            F -> ag
            E -> 0.0
            N -> 90.0
            W -> 180.0
            S -> -90.0
          (i, j) = getDirection iag

move :: Ship -> Instruction -> Ship
move (Ship ag loc w pt) (a, n)
    | elem a [R, L] = Ship nag loc w (pt ++ [(a, n)])
    | otherwise = Ship ag nloc w (pt ++ [(a, n)])
        where nag = rotate ag (a, n)
              nloc = translate ag loc (a, n)

manhattan :: Location -> Int
manhattan (x, y) = abs x + abs y

solve :: Ship -> [Instruction] -> Int
solve s [] = manhattan $ location s
solve s (i:is) = solve (move s i) is

main :: IO ()
main = do
    { args <- getArgs
    ; content <- parseFromFile instructions $ head args
    ; case content of
        Left err -> print err
        Right cs -> putStrLn $ "Part 1: " ++ show (solve newShip cs)
    }
