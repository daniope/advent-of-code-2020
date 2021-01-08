module Main where

import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)
import Data.Bits hiding (bit)
import Data.Maybe
import Text.Parsec
import Text.Parsec.String
import System.Environment

type BitMask = [(Int, Int)]
type Value = (Int, Int)

data Instruction = Instruction
    { mask :: BitMask
    , vals :: [Value]
    } deriving (Show)

type Program = [Instruction]

integer :: Parser Int
integer = fmap read $ many1 digit

bit :: Parser (Maybe Char)
bit = try (Just <$> digit) <|> (char 'X' >> return Nothing)

bitMask :: Parser BitMask
bitMask = do
    { _ <- string "mask = "
    ; bs <- many1 bit
    ; let l = length bs
    ; return [ (i, read [b]) | (i, Just b) <- zip [l-1,l-2..0] bs ]
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

encode :: Value -> BitMask -> Value
encode v [] = v
encode (loc, x) ((i, b):bm) 
    | b == 0 = encode (loc, clearBit x i) bm
    | b == 1 = encode (loc, setBit x i) bm

main :: IO ()
main = do
    { args <- getArgs
    ; result <- parseFromFile instruction $ head args
    ; case result of
        Left err -> print err
        Right r -> do
            { putStrLn $ "Part 1: " ++ show r
            }
    }
