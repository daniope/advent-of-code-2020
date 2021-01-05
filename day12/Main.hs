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
newShip = Ship 0.0 (0,0) (10,1)

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

rotateWaypoint :: Location -> Location -> Instruction -> Location
rotateWaypoint (x, y) (wx, wy) (a, n) = (nwx, nwy)
    where i = case a of
            R -> -1
            L -> 1
          nn = i * fromIntegral n :: Float
          nwx = cos' nn * wx - sin' nn * wy
          nwy = sin' nn * wx + cos' nn * wy

forward :: Location -> Location -> Instruction -> Location
forward (x, y) (wx, wy) (a, n) = (x + n * wx, y + n * wy)

move :: Part -> Ship -> Instruction -> Ship
move Part1 (Ship ag loc wloc) (a, n)
    | elem a [R, L] = Ship (rotate ag (a, n)) loc wloc
    | otherwise = Ship ag (translate ag loc (a, n)) wloc
move Part2 (Ship ag loc wloc) (a, n)
    | a == F = Ship ag (forward loc wloc (a, n)) wloc
    | elem a [R, L] = Ship ag loc (rotateWaypoint loc wloc (a, n))
    | otherwise = Ship ag loc (translate ag wloc (a, n))

manhattan :: Location -> Int
manhattan (x, y) = abs x + abs y

solve :: Part -> Ship -> [Instruction] -> Int
solve p s [] = manhattan $ point s
solve p s (i:is) = solve p (move p s i) is

main :: IO ()
main = do
    { args <- getArgs
    ; content <- parseFromFile instructions $ head args
    ; case content of
        Left err -> print err
        Right cs -> do
            { putStrLn $ "Part 1: " ++ show (solve Part1 newShip cs)
            ; putStrLn $ "Part 2: " ++ show (solve Part2 newShip cs)
            }
    }
