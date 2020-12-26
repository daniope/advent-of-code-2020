module Main where

import Data.List
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

none :: Parser [Content]
none = do
    { try $ string "no other bags."
    ; return []
    }

contents :: Parser [Content]
contents = none <|> many1 content <?> []

rule :: Parser Rule
rule = do
    { c   <- color
    ; _   <- string " contain "
    ; cts <- contents
    ; return (c, cts)
    }

rules :: Parser [Rule]
rules = endBy1 rule endOfLine

contains :: Color -> Rule -> Bool
contains c (cl, []) = False
contains c (cl, ct:cts) = (c == fst ct) || contains c (cl, cts)

matches :: Color -> [Rule] -> [Color]
matches c [] = []
matches c rs = [ fst r | r <- rs, contains c r]

allMatches :: Color -> [Rule] -> [Color]
allMatches c rs = union cls $ concat $ map (\x -> allMatches x rs) cls
    where cls = matches c rs

solve1 :: Color -> [Rule] -> Int
solve1 c rs = length $ allMatches c rs

add :: [Int] -> Int
add [] = 0
add (x:xs) = x + (add xs)

capacity :: [Content] -> Int
capacity cts = add $ map snd cts

solve2 :: Color -> [Rule] -> Int
solve2 c rs = capacity cts + n
    where cts = snd $ (filter (\x -> fst x == c) rs) !! 0
          f x = (snd x) * solve2 (fst x) rs
          n = add $ map f cts


main :: IO ()
main = do
    { args <- getArgs
    ; result <- parseFromFile rules $ head args
    ; case result of
        Left err -> print err
        Right rs -> do
            { putStrLn $ "Part 1: " ++ show (solve1 "shiny gold" rs)
            ; putStrLn $ "Part 2: " ++ show (solve2 "shiny gold" rs)
            }
    }
