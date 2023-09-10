{-# OPTIONS_GHC -Wall #-}
module Khomichenko01 where

-- Задача 1 -----------------------------------------
power3 :: [Integer]
power3 = map (^3) [1..] 

-- Задача 2 -----------------------------------------
toPower3 :: [Integer]
toPower3 = map (3^) [1..]

-- Задача 3 -----------------------------------------
sumPower3 :: Integer -> Integer
sumPower3 xs = sum (toPower3)

-- Задача 4 -----------------------------------------
sumPower :: Integer -> Integer -> Integer
sumPower = undefined

-- Задача 5 -----------------------------------------
lessMe :: [Int] -> [Int]
lessMe = undefined 
 
-- Задача 6 -----------------------------------------
hailstone :: Int -> Int
hailstone = undefined

-- Задача 7 -----------------------------------------
hailSeq :: Int -> [Int]
hailSeq = undefined

-- Задача 8 -----------------------------------------
allHailSeq :: [[Int]]
allHailSeq = undefined

-- Задача 9 -----------------------------------------
firstHailSeq :: Int -> Int
firstHailSeq = undefined
