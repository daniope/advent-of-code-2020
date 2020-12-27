module Main where

import Text.Parsec
import Text.Parsec.String
import System.Environment

data Operation =
      ACC -- Accumulate
    | JMP -- Jump
    | NOP -- None
    deriving (Show)

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
argument = read <$> (plus <|> minus <|> number)
    where plus   = char '+' *> number
          minus  = (:) <$> char '-' <*> number
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

main :: IO ()
main = do
    { args <- getArgs
    ; input <- parseFromFile commands $ head args
    ; case input of
        Left err -> print err
        Right cs -> do
            { putStrLn $ show cs
            }
    }
