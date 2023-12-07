{-# OPTIONS_GHC -Wall #-}
module Khomichenko09 where

import Data.List
import Data.Tree

-- Задача 1 -----------------------------------------			   
rank :: Tree a -> Int
rank b = length (subForest b)

-- Задача 2-----------------------------------------

calculateNodeNumber:: Tree a -> Int
calculateNodeNumber tree = if (length (subForest tree) == 0)
                        then 1
                    else 1 + sum (map (\el -> calculateNodeNumber el) (subForest tree))

isBinomTree :: Ord a => Tree a -> Bool
isBinomTree ts = 2 ^ (rank ts) == calculateNodeNumber ts

-- Задача 3 -----------------------------------------

isBinomHeap :: Ord a => Forest a -> Bool
isBinomHeap fs = all (== True) (map (\tree -> isBinomTree tree) fs)

-- Задача 4 -----------------------------------------
-- combineTrees :: Ord a => Tree a -> Tree a -> Tree a
combineTrees tree treeToCombine = if (rootLabel tree < rootLabel treeToCombine)
                                        then Node (rootLabel tree) ([treeToCombine] ++ (subForest tree) ) 
                                  else Node (rootLabel treeToCombine) ([tree] ++ (subForest treeToCombine) )
        

-- Задача 5 -----------------------------------------
extractMin :: Ord a => Forest a -> a
extractMin forest = minimum (map (rootLabel) forest)

-- Задача 6-----------------------------------------
mergeHeaps :: Ord a => Forest a -> Forest a -> Forest a
mergeHeaps h []= h
mergeHeaps [] heap= heap
mergeHeaps h@(t:ts) heap@(t':ts')| rank t < rank t' = t : mergeHeaps ts heap| rank t' < rank t = t' : mergeHeaps ts' h| otherwise = mergeHeaps (mergeHeaps ts ts') [combineTrees t t']

-- Задача 7-----------------------------------------
insert :: Ord a => a -> Forest a -> Forest a
insert value forest = mergeHeaps [Node value []] forest

-- Задача 8-----------------------------------------
deleteMin :: Ord a => Forest a -> Forest a
deleteMin heap = mergeHeaps heapp mrh 
        where
                mrt = head $ dropWhile (\t -> rootLabel t /= extractMin heap) heap
                mrh = reverse $ subForest mrt
                heapp = filter (\t -> t /= mrt) heap


-- Задача 9-----------------------------------------
binomSort :: Ord a => [a] -> [a]
binomSort [] = []
binomSort bs = minValue : binomSort (delete (minValue) (bs))
    where
      heap (b:bs) = Node b [] : heap bs
      heap [] = []
      minValue = extractMin $ heap bs

-- Задача 10 -----------------------------------------
toBinary :: Forest a -> [Int]
toBinary h = reverse $ treeBinary [0..] h
        where
                treeBinary _ [] = []
                treeBinary (n:ns) t'@(t:ts)| n == rank t = 1 : (treeBinary ns ts)| otherwise   = 0 : (treeBinary ns t')

-----------------------------------------------------  
-- Приклади деяких дерев...

t1, t2, t3, t4, t5, t6, t7, t8 :: Tree Int
--  Зауваження: t7 - результат злиття t5 і t6

-- t1 .. t4 з'являються на Мал. 1...
t1 = Node 4  []
t2 = Node 1 [Node 5 []]
t3 = Node 2 [Node 8 [Node 9 []], 
             Node 7 []]
t4 = Node 2 [Node 3 [Node 6 [Node 8 []], 
                     Node 10 []],
             Node 8 [Node 9 []],
             Node 7 []]

-- t5 і t6 зліва на Мал.2; t7 - справа на Мал.2
t5 = Node 4 [Node 6 [Node 8 []], 
                     Node 10 []]
t6 = Node 2 [Node 8 [Node 9 []], Node 7 []]
t7 = Node 2 [Node 4 [Node 6 [Node 8 []], Node 10 []],
             Node 8 [Node 9 []], 
             Node 7 []]

-- Додаткове дерево...
t8 = Node 12 [Node 16 []]

------------------------------------------------------
-- Приклади деяких куп...

h1, h2, h3, h4, h5, h6, h7 :: Forest Int
-- Two arbitrary heaps for testing...
h1 = [t2, t7]
h2 = [Node 1 [Node 12 [Node 16 []],
              Node 5 []],
      Node 2 [Node 4 [Node 6 [Node 8 []],
                      Node 10 []],
              Node 8 [Node 9 []],
              Node 7 []]]

-- h3 показана на Мал.3...
h3 = [t1, t2, t4]

-- Дві додаткові купи використовуються далі. Вони зліва на Мал.4(a)...

h4 = [t2, t5]
h5 = [t1, t8]

-- h6 - результат злиття h4 і h5, справа на Мал.4(b)...
h6 = [Node 4 [],
      Node 1 [Node 4 [Node 6  [Node 8 []],
                      Node 10 []],
              Node 12 [Node 16 []],
              Node 5 []]]

-- h7 показана на Мал.5...
h7 = [Node 4 [Node 4 [Node 12 [Node 16 []],
                      Node 5 []],
              Node 6 [Node 8 []],
              Node 10 []]]  


