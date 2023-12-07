{-# OPTIONS_GHC -Wall #-}
module Khomichenko03 where

-- Задача 1 -----------------------------------------
myButLast :: [Int] -> Int
myButLast [] = 0
myButLast [_] = 0
myButLast xs = xs !! (length xs - 2) 


-- Задача 2 -----------------------------------------
isPalindrom :: Int -> Bool
isPalindrom n = show n == reverse (show n)

palindrom10 :: Int -> Int -> [Int] 
palindrom10 n m = filter isPalindrom [n+1..m-1] 

-- Задача 3 -----------------------------------------
phi :: Int -> Int
phi n = length [x | x <- [1..n], gcd x n == 1]

-- Задача 4 -----------------------------------------
maxSuf :: [Int] -> Int
maxSuf xs = maximum ( map(\toDrop -> sum (drop toDrop xs)) [0..length xs - 1] )

-- Задача 5 -----------------------------------------
group :: Eq a => [a] -> [[a]]
group [] = []
group (x:xs) = (x : takeWhile (== x) xs) : group (dropWhile (== x) xs)

encode :: String -> [(Int, Char)]
encode xs = map (\grouped -> (length grouped, head grouped)) (group xs)

-- Задача 6 -----------------------------------------
isPrime :: Int -> Bool
isPrime n = n > 1 && all (\x -> n `mod` x /= 0) [2..sqrtN]
  where
    sqrtN = floor (sqrt (fromIntegral n))

goldbach :: Int -> (Int, Int)
goldbach n
  | n <= 2 || odd n = (0, 0)
  | otherwise = head [(x, y) | x <- primes, y <- primes, x + y == n]
  where
    primes = filter isPrime [2..n]


-- Задача 7 -----------------------------------------
maxComSuf :: String -> String -> Int
maxComSuf xs ys = length (takeWhile (\(x, y) -> x == y) (zip (reverse xs) (reverse ys)))

-- Задача 8 -----------------------------------------
groupChar :: String -> [String] 
groupChar xs = group xs

