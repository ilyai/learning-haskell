module Main where

import Data.Monoid

-- head :: [a] -> a
-- head (x:_) = x
-- head [] = error "head of empty list"

head :: Monoid a => [a] -> a
head (x:xs) = x
head [] = mempty

example :: [[Int]]
example = []

length :: Int
length = 8

doubleLength :: Int
doubleLength = Main.length * 2
