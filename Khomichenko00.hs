{-# OPTIONS_GHC -Wall #-}
module Khomichenko00 where

import Data.List (sort)

type Algorithm    = [Substitution]
type Substitution = (String,String,Bool)
type ConfigA      = (Bool, Int, String)

type Graph  = [[Int]]

data Tree23 a  = Leaf a   
               | Node2 (Tree23 a) a (Tree23 a) 
               | Node3 (Tree23 a) a (Tree23 a) a (Tree23 a)
               | Empty23     -- порожнє 2-3-дерево!!!
                   deriving (Eq, Show)

-- Задача 1 ------------------------------------
isPrefix :: String -> String -> Bool
isPrefix "" _ = True
isPrefix _ "" = False
isPrefix (b:bs) (x:xs) = b == x && isPrefix bs xs

-- Задача 2 ------------------------------------
substitute :: Substitution -> Int -> String -> String
substitute (old, new, _) i w = take i w ++ new ++ drop (i + length old) w

-- Задача 3------------------------------------
findPosition :: String -> Substitution -> [(Substitution,Int)] 
findPosition w sub@("", _, _) = map (\i -> (sub, i)) [0..length w]
findPosition w sub@(findString,_, _) = map (\(correnctPosition, i) -> (sub, i))
                                          (filter (\(correnctPosition, i) -> correnctPosition)
                                                 (map (\i -> (isPrefix findString (drop i w), i))[0..length w - 1]))

-- Задача 4 ------------------------------------
findAll :: Algorithm -> String -> [(Substitution, Int)]
findAll algo w = concatMap (\(sub, i) -> findPosition w sub) (zip algo [0..])

-- Задача 5 ------------------------------------
stepA :: Algorithm -> ConfigA -> ConfigA
step _ config@(False, _, _) = config
stepA algo config@(bt,st,word) = (not isEnd, st + 1,substitute sub i word)
              where
                     (sub@(_,_,isEnd), i) = ((findAll algo word)!!0)

-- Задача 6 ------------------------------------
evalA :: Algorithm -> Int -> String -> Maybe String
evalA algo m word = evalA' algo m (True, 0, word)
       where
              evalA' :: Algorithm -> Int -> ConfigA -> Maybe String
              evalA' _ _ (_, _, "") = Just ""
              evalA' [] _ (bt, st, word) = Just (word)
              evalA' (sub:subs) m config@(bt, st, word)
                     | st > m = Nothing
                     | not bt = Just word
                     | otherwise = evalA' algo m (stepA algo config)

-- Задача 7 -----------------------------------------
testing :: [Int] -> Bool
testing [] = True
testing [_] = True
testing (x:y:xs) = x <= y && testing (y:xs)

-- Задача 8 -----------------------------------------
primeFactor :: Int -> [Int]
primeFactor n = factorize n 2
       where
              factorize :: Int -> Int -> [Int]
              factorize 1 _ = []
              factorize num divisor
                     | num `mod` divisor == 0 = divisor : factorize (num `div` divisor) divisor
                     | otherwise = factorize num (divisor + 1)

-- Задача 9 -----------------------------------------
lastTail :: String -> String
lastTail xs = head(reverse (sort (suffixes xs)))

suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes xs'@(_:xs'') = xs' : suffixes xs''

-- Задача 10 ------------------------------------
sumPalindrom2 :: Int -> Int
sumPalindrom2 n = sum [x | x <- [1..n], isPalindrome (toBinary x)]

isPalindrome :: String -> Bool
isPalindrome s = s == reverse s

toBinary :: Int -> String
toBinary 0 = "0"
toBinary 1 = "1"
toBinary n = toBinary (n `div` 2) ++ show (n `mod` 2)

-- Задача 11 ------------------------------------
contains :: Eq a => a -> [a] -> Bool
contains = \elem -> \myList ->
       case myList of
              [] -> False
              x:xs | x == elem -> True
              _:xs -> contains elem xs

isTournament :: Graph -> Bool
isTournament gr = all (== True) (concatMap (\i -> (map (\j -> if i == j then True else ((contains j (gr!!i)) || (contains i (gr!!j)) ) ) [i..length gr - 1])) [0..length gr - 1])

-- Задача 12 ------------------------------------
findPaths :: [[Int]] -> Int -> Int -> [[Int]]
findPaths gr pointIndex1 pointIndex2 = findPaths' gr pointIndex1 pointIndex2 []

findPaths' :: [[Int]] -> Int -> Int -> [Int] -> [[Int]]
findPaths' gr pointIndex1 pointIndex2 path
       | pointIndex1 == pointIndex2 = [path ++ [pointIndex2]]
       | pointIndex1 `elem` path = [] 
       | otherwise = concatMap (\nextPoint -> findPaths' gr nextPoint pointIndex2 (path ++ [pointIndex1])) (gr !! pointIndex1)

findLongestPath :: [[Int]] -> Maybe [Int]
findLongestPath [] = Nothing
findLongestPath paths = Just (sort paths !! 0)

findClosestPath :: [[Int]] -> Maybe [Int]
findClosestPath [] = Nothing
findClosestPath paths = Just (sort paths !! (length paths - 1))


longWay :: Graph -> Int -> Int -> Maybe [Int]
longWay gr a b = findLongestPath (findPaths gr a b)

-- Задача 13 ------------------------------------
gamiltonWay :: Graph -> Maybe [Int]
gamiltonWay gr
              | null cycles = Nothing
              | otherwise = Just (head cycles)
       where
              cycles = filter (\cycle -> isHamiltonianCycle cycle gr) (allCycles gr)

isHamiltonianCycle :: [Int] -> Graph -> Bool
isHamiltonianCycle cycle gr = all (\i -> i `elem` cycle) [0.. length gr -1]

allCycles :: [[Int]] -> [[Int]]
allCycles gr = concatMap (\i -> findCycles gr i i []) [0..length gr - 1]

findCycles :: [[Int]] -> Int -> Int -> [Int] -> [[Int]]
findCycles gr start cur path
       | cur == start && length path > 1 = [path ++ [cur]]
       | cur `elem` path = []
       | otherwise = concatMap (\next -> findCycles gr start next (path ++ [cur])) (gr !! cur)


-- Задача 14 ------------------------------------
isAcyclic :: Graph -> Bool 
isAcyclic gr = length (allCycles gr) == 0

-- Задача 15 ------------------------------------
topolSort :: Graph -> Maybe [Int]
topolSort gr
       | isAcyclic gr = Just (topologicalSort gr)
       | otherwise = Nothing

topologicalSort :: Graph -> [Int]
topologicalSort gr = (dfs [] gr)

dfs :: [Int] -> Graph -> [Int]
dfs visited gr
       | length visited == length gr = visited
       | otherwise = dfs (next : visited) gr
       where
              next = head [v | v <- [0..length gr - 1], v `notElem` visited, all (`elem` visited) (gr !! v)]

-- Задача 16 ------------------------------------
isTopolSort :: Graph -> [Int] -> Bool 
isTopolSort _ [] = True
isTopolSort gr (t : ts) = (isTopolSort gr ts) && (all (\t -> elem t ts) (gr !! t))

-- Задача 17 ------------------------------------
isTree23  :: (Ord a) => Tree23 a -> Bool 
isTree23 t =
       let l = treeToList t
              in case l of
       Just l' -> isSorted l'
       Nothing -> False

isSorted :: (Ord a) => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x : y : xs) = if x <= y then isSorted (y : xs) else False

treeToList :: (Ord a) => Tree23 a -> Maybe [a]
treeToList Empty23 = Just []
treeToList (Leaf x) = Just [x]
treeToList (Node2 l x r) = case (treeToList l, treeToList r) of
(Just l', Just r') -> Just (l' ++ [x] ++ r')
_ -> Nothing
treeToList (Node3 l x m y r) = case (treeToList l, treeToList m, treeToList r) of
(Just l', Just m', Just r') -> Just (l' ++ [x] ++ m' ++ [y] ++ r')
_ -> Nothing

-- Задача 18 ------------------------------------
eqTree23 :: (Ord a) => Tree23 a -> Tree23 a -> Bool
eqTree23 t1 t2 =
       let l1 = treeToList t1
              l2 = treeToList t2
       in case (l1, l2) of
       (Just l1', Just l2') -> l1' == l2'
       _ -> False


-- Задача 19 ------------------------------------
elemTree23 :: (Ord a) => Tree23 a -> a -> Bool
elemTree23 (Leaf x) v = x == v
elemTree23 (Node2 l x r) v
       | v < x     = elemTree23 l v
       | v == x    = True
       | otherwise = elemTree23 r v
elemTree23 (Node3 l x m y r) v
       | v < x     = elemTree23 l v
       | v == x    = True
       | v < y     = elemTree23 m v
       | v == y    = True
       | otherwise = elemTree23 r v

-- Задача 20 ------------------------------------
insTree23 :: (Ord a) => Tree23 a -> a -> Tree23 a
insTree23 = undefined
--- Послання для майбутніх курсів що будуть шукати це завдання у мене.
--- Я їбав в рот це робити заради пари балів, мої колеги по курсу і минулих курсів так само
--- Тому цього завдання тут нема, ідіть нахуй далі шукати по іншим репам. Удачі


---------------------Тестові дані - Нормальні алгоритми Маркова ---------------------------
clearBeginOne, addEnd, reverseW, multiply:: Algorithm 
-- стирає перший символ вхідного слова (алфавіт {a,b})
clearBeginOne = [ ("ca", "", True)
                , ("cb", "", True)
                , ("", "c", False)
                ] 

-- дописує abb в кінець вхідного слова (алфавіт {a,b})
addEnd = [ ("ca", "ac", False)
         , ("cb", "bc", False)
         , ("c", "abb", True)
         , ("", "c", False)
         ] 
-- зеркальне відображення вхідного слова (алфавіт {a,b})
reverseW = [ ("cc", "d", False)
          , ("dc", "d", False)
          , ("da", "ad", False) 
          , ("db", "bd", False) 
          , ("d", "", True) 
          , ("caa", "aca", False) 
          , ("cab", "bca", False) 
          , ("cba", "acb", False)
          , ("cbb", "bcb", False) 
          , ("", "c", False) 
          ]

-- добуток натуральних чисел 
--  multiply ("|||#||") = "||||||"  3*2 = 6
multiply = [("a|", "|ba", False)
            ,("a", "", False)
            ,("b|", "|b", False)
            ,("|#", "#a", False)
            ,("#", "c", False)
            ,("c|", "c", False)
            ,("cb", "|c", False)
            ,("c", "", True)
            ]

---------------------Тестові дані - Графи -------
gr1, gr2, gr3, gr4:: Graph
gr1 = [[1,2,3],[2,3],[3,4],[4],[]]
gr2 = [[3,4],[0,3],[0,1,4],[2,4],[1]]
gr3 = [[1],[2],[3],[1],[0,3]]
gr4 = [[1,2,3],[1,2,3],[1,2,3],[1,2,3],[0,1,2,3]]

---- Тестові дані  2-3-дерева
tr1, tr2, tr3, tr4,tr5 :: Tree23 Int
tr1 =  Node2 (Node2 (Node2 (Leaf 0) 1 (Leaf 1)) 
                     2
                    (Node2 (Leaf 2) 3 (Leaf 3)))
              4
             (Node2 (Node2 (Leaf 4) 5 (Leaf 5)) 
                     6
                    (Node2 (Leaf 6) 7 (Leaf 7)))
tr2 =  Node3 (Node2 (Leaf 0) 1 (Leaf 1))
              2
             (Node3 (Leaf 2) 3 (Leaf 3) 4 (Leaf 4))
              5
             (Node3 (Leaf 5) 6 (Leaf 6) 7 (Leaf 7))

tr3 = Node3 (Node2 (Leaf 2) 5 (Leaf 5))
            7
            (Node3 (Leaf 7) 8 (Leaf 8) 12 (Leaf 12))
            16
            (Node2 (Leaf 16) 19 (Leaf 19))

tr4 = Node3 (Node2 (Leaf 2) 5 (Leaf 5))
            7
            (Node3 (Leaf 7) 8 (Leaf 8) 12 (Leaf 12))
            16
            (Node3 (Leaf 16) 18 (Leaf 18) 19 (Leaf 19))

tr5 = Node2 (Node2 (Node2 (Leaf 2) 5 (Leaf 5))
                    7
                   (Node2 (Leaf 7) 8 (Leaf 8)) 
            )
            10
            (Node2 (Node2 (Leaf 10) 12 (Leaf 12))
                   16
                   (Node3 (Leaf 16) 18 (Leaf 18) 19 (Leaf 19))
            )

