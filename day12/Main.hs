module Main where

import Text.Parsec
import Text.Parsec.String
import System.Environment

data Part
    = Part1
    | Part2

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
    , point     :: Location
    , wayPoint  :: Location
    } deriving (Show)

newShip :: Ship
newShip = Ship 0.0 (170,38) (180,42)

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

cos' :: Angle -> Int
cos' ag = round $ cos (ag * pi / 180.0)

sin' :: Angle -> Int
sin' ag = round $ sin (ag * pi / 180.0)

getDirection :: Angle -> Direction
getDirection n = (cos' n, sin' n)

rotate :: Ship -> Instruction -> Ship
rotate (Ship ag loc w) (a, n) = Ship nag loc w
    where i = case a of
            R -> -1
            L -> 1
          nag = ag + i * (fromIntegral n :: Float)

translate :: Ship -> Instruction -> Ship
translate (Ship ag (x, y) w) (a, n) = Ship ag nloc w
    where iag = case a of
            F -> ag
            E -> 0.0
            N -> 90.0
            W -> 180.0
            S -> -90.0
          (i, j) = getDirection iag
          nloc = (x + i * n, y + j * n)

rotateWaypoint :: Ship -> Instruction -> Ship
rotateWaypoint (Ship ag (x, y) (wx, wy)) (a, n) = Ship ag (x, y) (nwx, nwy)
    where i = case a of
            R -> -1
            L -> 1
          nn = i * fromIntegral n :: Float
          nwx = cos' nn * (wx - x) - sin' nn * (wy - y) + x
          nwy = sin' nn * (wx - x) + cos' nn * (wy - y) + y

move :: Ship -> Instruction -> Ship
move s (a, n) = f s (a, n)
    where f = if elem a [R, L]
            then rotate
            else translate

manhattan :: Location -> Int
manhattan (x, y) = abs x + abs y

solve :: Part -> Ship -> [Instruction] -> Int
solve p s [] = manhattan $ point s
solve Part1 s (i:is) = solve Part1 (move s i) is
solve Part2 s (i:is) = solve Part2 (move s i) is

main :: IO ()
main = do
    { args <- getArgs
    ; content <- parseFromFile instructions $ head args
    ; case content of
        Left err -> print err
        Right cs -> do
            { putStrLn $ "Part 1: " ++ show (solve Part1 newShip cs)
            ; putStrLn $ "Part 2: " ++ show (solve Part2 newShip cs)
            ; putStrLn $ "Part 2: " ++ show (rotateWaypoint newShip (R,180))
            }
    }
