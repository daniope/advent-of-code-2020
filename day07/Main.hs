module Main where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import System.Environment

type Color = String
type Content = (Color, Int)
type Rule = (Color, [Content])

color :: Parser Color
color = do
    { c1 <- many1 letter
    ; _  <- space
    ; c2 <- many1 letter
    ; _  <- space
    ; _  <- string "bag"
    ; _  <- optional $ char 's' 
    ; return (c1 ++ " " ++ c2)
    }

quantity :: Parser Int
quantity = fmap read $ many1 digit

content :: Parser Content
content = do
    { n <- quantity
    ; _ <- space
    ; c <- color
    ; _ <- optional $ string ", "
    ; _ <- optional $ char '.'
    ; return (c, n)
    }

contents :: Parser [Content]
contents = do
    { cts <- optionMaybe $ many1 content
    ; case cts of
        Just c -> return c
        Nothing -> do
            { _ <- string "no other bags."
            ; return []
            }
    }

rule :: Parser Rule
rule = do
    { c   <- color
    ; _   <- string " contain "
    ; cts <- contents
    ; return (c, cts)
    }

rules :: Parser [Rule]
rules = endBy1 rule endOfLine

main :: IO ()
main = do
    { args <- getArgs
    ; result <- parseFromFile rules $ head args
    ; case result of
        Left err -> print err
        Right bs
            -> putStrLn $ "Part 1: " ++ show bs
    }
