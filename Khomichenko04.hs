{-# OPTIONS_GHC -Wall #-}
module HWC04 where

-- Код - просто список символів - десяткових цифр '0' ..'9'
type Code = String

-- Крок гри (Move) будує конструктор Move використовуючи спробу (Code) і два цілих:  
--    кількість "биків" і "корів"  у пропозиції-спробі по відношенню до коду-числа 
data Move = Move Code Int Int
          deriving (Show, Eq)

-- Задача 1 -----------------------------------------
exactMatches :: Code -> Code -> Int
exactMatches = undefined

-- Задача 2 -----------------------------------------
countDigits :: Code -> [Int]
countDigits = undefined

-- Задача 3 ----------------------------------------- 
matches :: Code -> Code -> Int
matches = undefined
 
-- Задача 4 -----------------------------------------
getMove :: Code -> Code -> Move
getMove = undefined

-- Задача 5 -----------------------------------------
isConsistent :: Move -> Code -> Bool
isConsistent = undefined

-- Задача 6 -----------------------------------------
filterCodes :: Move -> [Code] -> [Code]
filterCodes = undefined

-- Задача 7 -----------------------------------------
allCodes :: Int -> [Code]
allCodes = undefined
   
-- Задача 8 -----------------------------------------
solve :: Code -> [Move]
solve = undefined
 
