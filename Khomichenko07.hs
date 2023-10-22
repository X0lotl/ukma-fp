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
solveSimple = undefined

-- Задача 3.a -----------------------------------------
findRow :: Linear -> Maybe Int
findRow = undefined

-- Задача 3.b -----------------------------------------
exchangeRow :: [a] -> Int -> [a]
exchangeRow = undefined

-- Задача 4.a -----------------------------------------
forwardStep :: Row -> Linear -> Linear
forwardStep = undefined

-- Задача 4.b -----------------------------------------
reverseStep :: Row -> Solution -> Solution
reverseStep = undefined

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

