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
isSimple le =
    all (\row -> length row == 1) le
    ||
    (
      (length le == 1)
      &&
      (head (head le) /= 0)
    )

-- Задача 2.b -----------------------------------------
solveSimple :: Linear -> Solution
solveSimple le =
    if all (\row -> length row == 1) le
      then
        if all (\row -> head row == 0) le
          then Just (Just [])
        else Nothing
    else
      if length (head le) == 2
        then Just (
          Just [last (head le) / head (head le)]
        )
      else
        Just Nothing


-- Задача 3.a -----------------------------------------
findRow :: Linear -> Maybe Int
findRow le = case filter (\row -> head row /= 0) le of
        [] -> Nothing
        (x:_) -> Just (1 + length (takeWhile (/= x) le))

-- Задача 3.b -----------------------------------------
exchangeRow :: [a] -> Int -> [a]
exchangeRow arr i =
  if i == 1 then arr
  else
    (arr!!(i - 1)) : take (i - 2) (drop 1 arr)
    ++
    head arr : drop i arr

-- Задача 4.a -----------------------------------------
forwardStep :: Row -> Linear -> Linear
forwardStep row le =
  if head row == 0
    then tail row : map tail le
  else
  map (
    \m -> (
      map (
        \k -> (
          ( (le!!m)!!k - ( head (le!!m) / head row ) * row!!k )
          )
      ) [1..(length row - 2)] ++ [ 
        (last (le!!m)) - ( ( head (le!!m) / (head row) ) * (last row) )
      ]
    )
  ) [0..(length le - 1)]

-- Задача 4.b -----------------------------------------
wrapSolution :: Solution -> (Row -> Row ) -> Solution
wrapSolution solution fn =
  case solution of
    Nothing -> Nothing
    Just justSolution ->
      case justSolution of 
        Nothing -> Just Nothing
        Just [] -> Just Nothing
        Just solutionRow -> Just (Just (fn solutionRow))

reverseStep :: Row -> Solution -> Solution
reverseStep row solution = 
  wrapSolution solution (\solutionRow ->
    (
      ((last row) / (head row)) 
      - sum (
        map
        (\(p, index) -> 
          ( 
            (row!!(index + 1)) / (head row)
          )
          * p 
        )
        (zip solutionRow [0..(length solutionRow - 1)]))
    ) : solutionRow
  )
-- Задача 5 -----------------------------------------
normalizeStep :: Linear -> Solution
normalizeStep le =
  reverseStep 
    (head le)
    (gauss (forwardStep (head le) (tail le)))

gauss :: Linear -> Solution
gauss le = 
  if isSimple le
    then solveSimple le
  else 
    case (findRow le) of
      Nothing -> normalizeStep le
      Just index -> 
        normalizeStep (exchangeRow le index)

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