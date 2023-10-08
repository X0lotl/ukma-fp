{-# OPTIONS_GHC -Wall #-}
module Khomichenko05 where

type GraphS = (Int,[(Int,Int)])            
type Graph  = [[Int]]

-- Задача 1 ------------------------------------
isOrdinary :: Graph -> Bool 
isOrdinary graph = all (\point -> all (\x -> length (filter (==x) point) <= 1) point) graph
                && all (\(u, v) -> (v, u) `elem` edges) edges
  where
    edges = [(u, v) | (u, vs) <- zip [0..] graph, v <- vs]

-- Задача 2 ------------------------------------
fromGraph :: Graph -> GraphS 
fromGraph graph = (numVertices, edges)
  where
    numVertices = length graph - 1
    edges = [(u, v) | (u, vs) <- zip [0..] graph, v <- vs]

-- Задача 3 ------------------------------------
toGraph :: GraphS -> Graph
toGraph (numVertices, edges) = map (\i -> map snd (filter (\(u, v) -> u == i) edges)) [0..numVertices]

-- Задача 4 ------------------------------------
shortWay :: Graph -> Int -> Int -> [Int]
shortWay graph start end = reverse( bfs [(start, [])] [])
  where
    bfs [] _ = []
    bfs ((v, path):q) visited
      | v `elem` visited = bfs q visited
      | v == end = (v:path)
      | otherwise = bfs (q ++ [(x, v:path) | x <- graph !! v]) (v:visited)

-- Задача 5 ------------------------------------
isConnecting :: Graph -> Bool 
isConnecting graph = length (components graph) == 1

-- Задача 6 ------------------------------------
components :: Graph -> [[Int]]
components graph = go [] [] [0..numVertices]
  where
    numVertices = length graph - 1
    go acc _ [] = acc
    go acc visited (v:vs)
      | v `elem` visited = go acc visited vs
      | otherwise = go (component:acc) (visited ++ component) vs
      where
        component = dfs [v] []
        dfs [] acc' = acc'
        dfs (x:xs) acc'
          | x `elem` acc' = dfs xs acc'
          | otherwise = dfs (xs ++ graph !! x) (x:acc')

-- Задача 7 ------------------------------------
eccentricity :: Graph -> Int -> Int 
eccentricity graph v = maximum (map (\x -> length (shortWay graph v x) - 1) [0..numVertices])
  where
    numVertices = length graph - 1

-- Задача 8 ------------------------------------
findDiameter :: Graph -> Int 
findDiameter graph = maximum [length (shortWay graph u v) - 1 | u <- [0..numVertices], v <- [0..numVertices]]
  where
    numVertices = length graph - 1

findRadius :: Graph -> Int 
findRadius graph = minimum [maximum [length (shortWay graph u v) - 1 | v <- [0..numVertices]] | u <- [0..numVertices]]
  where
    numVertices = length graph - 1

-- Задача 9 ------------------------------------
findCenter :: Graph -> [Int] 
findCenter graph = filter (\x -> eccentricity graph x == radius) [0..numVertices]
  where
    numVertices = length graph - 1
    radius = findRadius graph

-- Задача 10 ------------------------------------
shortWayAll :: Graph -> Int -> Int -> [[Int]]
shortWayAll graph start end = filter (\elem -> length elem == shortestWayLength ) (map (\i -> shortWay (map (filter (\el-> el /= i)) graph) start end) (shortWay graph start end))
  where
    shortestWayLength = length (shortWay graph start end)

---------------------Тестові дані - Графи -------
gr1S, gr2S:: GraphS
gr1S = (5,[(0,1),(0,2),(0,3),(1,0),(1,3),(1,4),
           (2,0),(2,4),(2,5),(3,0),(3,1),(4,1),(4,2),(5,2)])
gr2S = (7,[(0,1),(0,3),(1,0),(1,2),(2,1),(2,3),(3,0),(3,2),
           (4,5),(4,6),(5,4),(5,6), (6,4),(6,5)])

gr1, gr2:: Graph
gr1 = [[1,2,3],[0,3,4],[0,4,5],[0,1],[1,2],[2]]
gr2 = [[1,3],[0,2],[1,3],[0,2],[5,6],[4,6],[4,5],[]]
