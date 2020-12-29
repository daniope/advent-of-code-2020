module Main where

import Data.List
import Text.Parsec
import Text.Parsec.String
import System.Environment

data Operation =
      ACC -- Accumulate
    | JMP -- Jump
    | NOP -- None
    deriving (Eq, Show)

type Argument = Int
type Command = (Operation, Argument)

data Stop =
      InfiniteLoop
    | EOF
    | None
    deriving (Show)

data Boot = Boot
    { accumulator :: Int
    , step        :: Int
    , history     :: [Int]
    , code        :: [Command]
    , stop        :: Stop
    } deriving (Show)

setStop :: Boot -> Stop -> Boot
setStop b s = Boot (accumulator b) (step b) (history b) (code b) s

operation :: Parser Operation
operation = do
    { o <- many1 letter
    ; return $ case o of
        "acc" -> ACC
        "jmp" -> JMP
        "nop" -> NOP
        _     -> error "Invalid operation"
    }

argument :: Parser Argument
argument = fmap read $ positive <|> negative
    where positive = char '+' *> number
          negative = (:) <$> char '-' <*> number
          number = many1 digit

command :: Parser Command
command = do
    { o <- operation
    ; _ <- space
    ; a <- argument
    ; return (o, a)
    }

commands :: Parser [Command]
commands = endBy1 command endOfLine

jump :: Command -> Int -> Int
jump (JMP, a) n = a + n
jump _        n = 1 + n

accumulate :: Command -> Int -> Int
accumulate (ACC, a) n = a + n
accumulate _        n = n

run :: Boot -> Boot
run b
    | curr >= size          = setStop b EOF
    | elem curr (history b) = setStop b InfiniteLoop
    | otherwise             = run $ Boot acc next hist (code b) None
    where size = length $ code b
          curr = step b
          c    = (code b) !! curr
          next = jump (code b !! curr) curr
          hist = history b ++ [curr]
          acc  = accumulate (code b !! curr) (accumulator b)

solve1 :: [Command] -> Boot
solve1 cs = run (Boot 0 0 [] cs None)

replace :: Int -> [Command] -> [Command]    
replace i cs = take i cs ++ [(o, snd c)] ++ drop (i+1) cs
    where c = cs !! i 
          o = case (fst c) of
              JMP -> NOP
              NOP -> JMP
              _   -> o

getSteps :: [Command] -> [Int]
getSteps cs = [ i | (c, i) <- zip cs [0..], fst c `elem` [JMP, NOP] ]

solve2 :: [Command] -> [Int] -> Boot
solve2 cs [] = run $ Boot 0 0 [] cs None
solve2 cs t = core $ getSteps cs
    where core (s:ss) = do
              { let b = run $ Boot 0 0 [] (replace s cs) None
              ; case (stop b) of
                  EOF -> b
                  _   -> core ss
              }

main :: IO ()
main = do
    { args <- getArgs
    ; input <- parseFromFile commands $ head args
    ; case input of
        Left err -> print err
        Right cs -> do
            { putStrLn $ "Part 1: " ++ show (accumulator $ solve1 cs)
            ; putStrLn $ "Part 2: " ++ show (accumulator $ solve2 cs $ getSteps cs)
            }
    }
