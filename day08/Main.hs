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

jump :: Command -> Int
jump (c, a)
    | c == JMP = a
    | otherwise = 1

accumulate :: Command -> Int
accumulate (c, a)
    | c == ACC = a
    | otherwise = 0

replace :: Int -> a -> [a] -> [a]
replace i x xs = take i xs ++ [x] ++ drop (i+1) xs

run :: [Command] -> [Bool] -> Int -> Int
run cs bs i
    | bs !! i = 0
    | otherwise = accumulate c + (run cs nbs j)
    where c = cs !! i
          j = i + jump c
          nbs = replace i True bs

solve1 :: [Command] -> Int
solve1 cs = do
    { let bs = [ False | x <- cs ]
    ; run cs bs 0
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
