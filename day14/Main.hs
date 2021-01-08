module Main where

import Text.Parsec
import Text.Parsec.String
import System.Environment

type BitMask = String
type Value = (Int, Int)

data Instruction = Instruction
    { mask :: BitMask
    , vals :: [Value]
    } deriving (Show)

type Program = [Instruction]

integer :: Parser Int
integer = fmap read $ many1 digit

bitMask :: Parser BitMask
bitMask = do
    { _ <- string "mask = "
    ; m <- many1 alphaNum
    ; return m
    }

value :: Parser Value
value = do
    { _ <- try $ string "mem["
    ; loc <- integer
    ; _ <- string "] = "
    ; num <- integer
    ; return (loc, num)
    }

instruction :: Parser Instruction
instruction = do
    { m <- bitMask
    ; _ <- endOfLine
    ; vs <- endBy value endOfLine
    ; return $ Instruction m vs
    }

program :: Parser Program
program = many1 instruction

main :: IO ()
main = do
    { args <- getArgs
    ; result <- parseFromFile program $ head args
    ; case result of
        Left err -> print err
        Right r -> do
            { putStrLn $ "Part 1: " ++ show r
            }
    }
