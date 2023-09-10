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
sumPower3 length = sum (map (3^) [1..length])

-- Задача 4 -----------------------------------------
sumPower :: Integer -> Integer -> Integer
sumPower base length = sum (map (base^) [1..length])

-- Задача 5 -----------------------------------------
lessMe :: [Int] -> [Int]
lessMe xs = map (\x -> length (filter (< x) xs)) xs 
 
-- Задача 6 -----------------------------------------
hailstone :: Int -> Int
hailstone n
    | even n    = n `div` 2
    | otherwise = 3 * n + 1

-- Задача 7 -----------------------------------------
hailSeq :: Int -> [Int]
hailSeq n = if n == 1 then [1] else n : hailSeq (hailstone n)

-- Задача 8 -----------------------------------------
allHailSeq :: [[Int]]
allHailSeq = map hailSeq [1..]

-- Задача 9 -----------------------------------------
firstHailSeq :: Int -> Int
firstHailSeq l = head [x | x <- [1 ..], length (hailSeq x) == l]
