{-# OPTIONS_GHC -Wall #-}
module Khomichenko08 where

data Stream a = Cons a (Stream a)

-- Екземпляр Show виводить перші 20 елементів, за якими розташовані крапки продовження
instance Show a => Show (Stream a) where
    show xs =  (foldl (++) "["
                  $ map (\x -> (show x) ++ ", ") $ take 20 $ streamToList xs
                ) ++ "..."

-- перетворює Stream в нескінечний список
streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

-- Задача 1 -----------------------------------------
sRepeat :: a -> Stream a
sRepeat a = Cons a (sRepeat a)

sIterate :: (a -> a) -> a -> Stream a
sIterate fn a = Cons a (sIterate fn (fn a))

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons curLeft nextLeft) right = Cons curLeft (sInterleave right nextLeft)

sTake :: Int -> Stream a -> [a]
sTake count (Cons cur next) = if count == 0 then [] else cur : sTake (count - 1) next

-- Задача 2 -----------------------------------------
nats :: Stream Integer
nats = sIterate (+ 1) 0

-- Задача 3 -----------------------------------------
instance Functor Stream where
    fmap fn (Cons cur next) = Cons (fn cur) (fmap fn next)

-- Задача 4 -----------------------------------------
extractNext :: Stream Integer -> Stream Integer
extractNext (Cons _ next) = next

ruler :: Stream Integer
ruler = fmap (\x -> last (
    filter (\y ->
      (x `mod` floor ((2 :: Double) ** fromIntegral y)) == 0
    ) [0 .. (floor (logBase (2 :: Double) (fromIntegral x)))])
  ) (extractNext nats)

-- Задача 5 -----------------------------------------
fib :: Integer -> Integer
fib n =
  if n == 1 || n == 2
    then 1
  else fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [1..]

-- Задача 6 -----------------------------------------
fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Задача 7 -----------------------------------------
data  Matrix a = M(a,a)(a,a)
         deriving (Show, Eq, Ord)

instance Num a => Num (Matrix a) where
    (+) (M (a00, a01)(a10, a11)) (M (b00, b01)(b10, b11)) = M (a00+b00, a01+b01) (a10+b10, a11+b11)
    (*) (M (a00, a01)(a10, a11)) (M (b00, b01)(b10, b11)) =
      M ((a00*b00)+(a01*b10),(a00*b01)+(a01*b11))
        ((a10*b00)+(a11*b10),(a10*b01)+(a11*b11))
    negate (M (a00, a01)(a10, a11)) = M (negate a00, negate a01) (negate a10, negate a11)
    fromInteger integer = M (fromInteger integer, fromInteger integer) (fromInteger integer, fromInteger integer)
    -- Реалізовувати не потрібно
    abs    = undefined
    signum = undefined
-- Для класу Num немає законів, 
--  але очікується, що тип екземпляр класу Num, 
--  є алгебраїчне кільце відносно операцій + і * 
--    + і * - асоціативні і комутативні операції 
--    fromInteger 0 - одиниця для операції +
--    fromInteger 1 - одиниця для операції *
-- Задача 8 ----------------------------------------
get00 :: Matrix a -> a
get00 (M (a00,_)(_,_)) = a00

fastFib :: Integer -> Integer
fastFib n
  | n == 1 = 1
  | n == 2 = 1
  | n == 3 = 2
  | otherwise = get00 (M (1,1) (1,0) ^ (n - 1))
