{-# OPTIONS_GHC -Wall #-}
module HWC04 where

-- Код - просто список символів - десяткових цифр '0' ..'9'
type Code = String

-- Крок гри (Move) будує конструктор Move використовуючи спробу (Code) і два цілих:  
--    кількість "биків" і "корів"  у пропозиції-спробі по відношенню до коду-числа 
data Move = Move Code Int Int
          deriving (Show, Eq)

convertStringToList :: Code -> [String]
convertStringToList cd = map (\c -> [c]) (cd)

-- Задача 1 -----------------------------------------
exactMatches :: Code -> Code -> Int
exactMatches cd att = length (
    filter(\el -> fst el == (convertStringToList(att) !! (snd el)))
      (zip (convertStringToList (cd)) [0..])
  )

-- Задача 2 -----------------------------------------
countDigits :: Code -> [Int]
countDigits cd = map (\i -> length (filter (==show(i)) (convertStringToList(cd)))) [0..9]

-- Задача 3 ----------------------------------------- 
matches :: Code -> Code -> Int
matches cd att = sum (map (\el -> min (fst el) (snd el)) (zip (countDigits(cd)) (countDigits(att))))

-- Задача 4 -----------------------------------------
getMove :: Code -> Code -> Move
getMove secret guess = Move guess numBulls numCows
  where
    numBulls = exactMatches secret guess
    numCows = (matches secret guess) - numBulls

-- Задача 5 -----------------------------------------
isConsistent :: Move -> Code -> Bool
isConsistent (Move guess bulls cows) cd = bullsCount == bulls && cowsCount == cows
  where
    bullsCount = exactMatches guess cd
    cowsCount = matches guess cd - bullsCount

-- Задача 6 -----------------------------------------
filterCodes :: Move -> [Code] -> [Code]
filterCodes mv cdx = filter (\cd -> isConsistent mv cd) cdx

-- Задача 7 -----------------------------------------
extend :: [String] -> Int -> [String]
extend cdx d = map (\cd -> show d ++ cd) cdx

allCodes :: Int -> [Code]
allCodes n
  | n <= 0 = []
  | n == 1 = map show [0..9]
  | otherwise = concatMap (extend (allCodes (n-1))) [0..9]

-- Задача 8 -----------------------------------------
solve :: Code -> [Move]
solve cd = recursiveCodeSolver cd (allCodes (length cd)) [Move (replicate (length cd) '0') 0 0]

recursiveCodeSolver :: Code -> [Code] -> [Move] -> [Move]
recursiveCodeSolver _ [] _ = []
recursiveCodeSolver cd (x:xs) prevMoves
    | correctGuess = newMove : recursiveCodeSolver cd (filterCodes newMove xs) (newMove : prevMoves)
    | otherwise = recursiveCodeSolver cd (filterCodes newMove xs) (newMove : prevMoves)
    where
        newMove = getMove cd x
        correctGuess = isConsistent newMove cd && not (newMove `elem` prevMoves)