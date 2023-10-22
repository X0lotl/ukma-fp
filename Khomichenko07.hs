{-# OPTIONS_GHC -Wall #-}
module Khomichenko07 where 

import Prelude

type Linear   = [Row]
type Row      = [Rational]
type Solution = Maybe (Maybe Row)
-- Система рівнянь не має розвязків      => Nothing 
-- Система рівнянь має безліч розвязків  => Just Nothing 
-- Система рівнянь має один розвязок     => Just(Just [Rational]) 

-- Задача 1 -----------------------------------------

testing :: Linear -> Row -> Bool
testing le row = all (\(l, r) -> sum (zipWith (*) l row) == last l) (zip le row)

-- Задача 2.a -----------------------------------------
isSimple :: Linear -> Bool
isSimple le = all (\el -> length el == 1) le || length le == length (head le) - 1

-- Задача 2.b -----------------------------------------
solveSimple :: Linear -> Solution
solveSimple le = if isSimple le then
              if all (\el -> length el == 1) le && head (head le) /= head (last le)
                  then Nothing 
                else if any (\el -> any (==0) el) (le) then Just Nothing 
                  else Just (Just [last (last (le))/head (head (le))] )
            else Nothing

-- Задача 3.a -----------------------------------------
findRow :: Linear -> Maybe Int
findRow le = case filter (\row -> head row /= 0) le of
        [] -> Nothing
        (x:_) -> Just (1 + length (takeWhile (/= x) le))

-- Задача 3.b -----------------------------------------
exchangeRow :: [a] -> Int -> [a]
exchangeRow le i = let (x:xs) = le in le !! (i-1) : xs ++ [x]

-- Задача 4.a -----------------------------------------
forwardStep :: Row -> Linear -> Linear
forwardStep fs rs = map (\row -> tail (zipWith (-) row (map (\el -> el * (head row / head fs )) fs ))) rs

-- Задача 4.b -----------------------------------------
reverseStep :: Row -> Solution -> Solution
reverseStep

-- Задача 5 -----------------------------------------
gauss :: Linear -> Solution
gauss = undefined

-------------------------------------------------------
test1, test2, test3, test4 :: Linear
test1 = [[6,1,2,21], [4,-6,16,2], [3,8,1,2]]
test2 = [[5,4,-2,3,11],[1,2,4,1,35],[6,6,2,4,46]]
test3 = [[2,3,4],[4,6,4]]
test4 = [[0,1,2,21], [0,-6,6,2], [2,2,1,2]]

row1, row2 :: Row 
row1 = [62/15, -17/15, -4/3]
row2 = [3,5,4]

res1, res2, res3 :: Solution
res1 = Just (Just row1) 
res2 = Just Nothing
res3 = Nothing

