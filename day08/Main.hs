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

replace :: Int -> a -> [a] -> [a]
replace i x xs = take i xs ++ [x] ++ drop (i+1) xs

run1 :: [Command] -> [Int] -> Int -> Int
run1 cs bs i
    | i >= length cs = 0
    | elem i bs = 0
    | otherwise = accumulate c $ run1 cs nbs j
    where c = cs !! i
          j = jump c i
          nbs = i : bs

solve1 :: [Command] -> Int
solve1 cs = do
    { let bs = [ False | x <- cs ]
    ; run1 cs [] 0
    }

main :: IO ()
main = do
    { args <- getArgs
    ; input <- parseFromFile commands $ head args
    ; case input of
        Left err -> print err
        Right cs -> do
            { putStrLn $ "Part 1: " ++ show (solve1 cs)
            }
    }
