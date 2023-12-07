{-# OPTIONS_GHC -Wall #-}
module Khomichenko10 where

import Data.Char(isUpper)
import Data.List
import Data.Maybe

type Grammar    = [Production]         -- КВ-граматика
type Production = (Char,String)        -- Правило виводу
type Predict    = [(Char,String)]      -- Прогнозуюча таблиця
type Control    = [((Char,Char),Int)]  -- Управляюча таблиця 

-- Задача 1 ------------------------------------
addOne :: String -> Char -> String  
addOne st c = if (find (==c) st) /= Nothing
              then st
              else st ++ [c]

addAll :: String -> String -> String 
addAll st wd = foldl addOne st wd 

addWithout :: String -> String -> String 
addWithout st wd = foldl (\acc el -> if (el == '$')
                                   then acc
                                   else addOne acc el) st wd

inter :: String -> String -> String 
inter st1 st2 = foldl (\acc el -> if (find (==el) st1) == Nothing
                                   then acc
                                   else addOne acc el) [] st2

-- Задача 2 ------------------------------------
tkPredict :: Predict -> Char -> String 
tkPredict pt n = 
    let filtered = filter (\(c, _) -> c == n) pt
    in 
       if null filtered then "" else snd (head filtered)

upPredict :: Predict -> Char -> String -> Predict 
upPredict pt n st = 
       let filtered = filter (\(c, _) -> c == n) pt
       in
       if null filtered then sort ((n,st):pt) else map (\(c,s) -> if c==n then (n,st)else (c,s)) pt
-- Задача 3 ------------------------------------

parse :: Grammar -> Control -> String -> Maybe [Int]
parse gr ctl word = case step gr ctl (word, ['S'], []) of
       (_, _, res) -> if null res then Nothing else Just res

step :: Grammar -> Control -> (String, String, [Int]) -> (String, String, [Int])
step _ _ ("", [], res) = ("", [], reverse res)
step _ _ ("", _, _) = ("", [], [])
step gr ctl (input, stack@(s:ss), res)
       | isUpper s = case lookup (s, head input) ctl of
                     Just i -> step gr ctl (input, (snd $ gr !! i) ++ tail stack, i:res)
                     Nothing -> ("", [], [])
       | s == head input = step gr ctl (tail input, ss, res)
       | otherwise = ("", [], [])

-- Задача 4 ------------------------------------
isNonTerm :: Char -> Bool
isNonTerm x = x `elem` ['A' .. 'Z']

isTerm :: Char -> Bool
isTerm x = not (isNonTerm x) && x /= '$'

first :: Predict -> String -> String
first pFst st
       | null st = "$"
       | (isTerm . head) st = [head st]
       | otherwise =
              let fstA = findFst pFst (head st)
                     in if length st == 1 || notElem '$' fstA
                            then fstA
                            else addWithout (first pFst (tail st)) fstA

findFst :: Predict -> Char -> String
findFst pr n = (snd . head) [p | p@(s, _) <- pr, s == n]


-- Задача 5 ------------------------------------
buildingControl :: Grammar -> Predict -> Predict -> Control
buildingControl gr pFst pNxt = [( (x, y), i) | (i, (x, r)) <- zip [0..] gr, y <- addAll (first (pFst) (r)) (if elem "" [first pFst r] then follow pNxt x else []) , isTerm y] 
       where follow p x = inter (foldl (\acc (a, b) -> if elem x b then addAll acc (tkPredict p a) else acc) [] gr) (tkPredict p x)


-- Задача 6 ------------------------------------

testingLL1 :: Grammar -> Predict -> Predict -> Bool
testingLL1 gr pFst pNxt = foldr (&&) True [testFst (map (first pFst) rls) && if (tkPredict pFst n) == "" then testFollow (tkPredict pNxt n) (map (first pFst) rls) else True | (n, rls) <- (fromGrammar gr)]

fromGrammar :: Grammar -> [(Char, [String])]
fromGrammar gr = map (\grp -> (fst (head grp), map snd grp)) groupedProductions
  where
    sortedProductions = sort gr 
    groupedProductions = groupBy sameFst sortedProductions
    sameFst a b = fst a == fst b   

testFst :: [String] -> Bool
testFst rls = foldl (&&) True [null (inter a b)| a<-rls, b<-(delete a rls)]

testFollow :: String -> [String] -> Bool
testFollow fs rls = foldl (&&) True [null (inter a fs)| a<-rls]

-- Задача 7 ------------------------------------
buildFst :: Grammar -> Predict
buildFst gr = untilConverges updateFirstTable initialFirstTable
       where
       nonTerminals = nub [nt | (nt, _) <- gr]
       initialFirstTable = [(nt, "") | nt <- nonTerminals]
       updateFirstTable ft = [(nt, nub . concat $ map (firstOfString ft) productions)
                           | nt <- nonTerminals, let productions = [rhs | (lhs, rhs) <- gr, lhs == nt]]
firstOfString :: Predict -> String -> String                    
firstOfString ft (y:ys)
      | not(isUpper y) = [y]
      | otherwise    = let firstY = tkPredict ft y
                       in if '$' `elem` firstY
                          then firstY ++ firstOfString ft ys
                          else firstY
firstOfString _ [] = ['$']

untilConverges :: Eq a => (a -> a) -> a -> a
untilConverges f x = let x' = f x in if x == x' then x else untilConverges f x'

-- Задача 8 ------------------------------------
buildNxt :: Grammar -> Predict -> Predict 
buildNxt gr pFst = fst (until (\x -> not (snd x)) stepNxt ((crtNxt gr), True))
    where
       stepNxt :: (Predict, Bool) -> (Predict, Bool) 
       stepNxt (prr, cnt) = evalNxt (nontermTails gr) pFst (prr, cnt)


nontermTails :: Grammar -> [(Char,String)]
nontermTails gr = concatMap (tailf1Prod) gr
    where
       tailf1Prod :: Production -> [(Char,String)]
       tailf1Prod (k, st) = [(k, o) | (ind, symb) <- (zip [0..] st), isUpper symb, let o = drop ind st]

evalNxt :: [(Char, String)] -> Predict -> (Predict, Bool) -> (Predict, Bool)
evalNxt tails' pFst (pNxt, _) =
    let npr = pssEx tails' pNxt
        pssEx :: [(Char, String)] -> Predict -> Predict
        pssEx [] pr = pr -- errr
        pssEx ((k, t):tx) pr = pssEx tx (extandNxtOne pFst k pr t)
    in if (pNxt == npr) then (npr, False)
       else (npr, True)


extandNxtOne :: Predict -> Char -> Predict -> String -> Predict
extandNxtOne pFst n pNxt (o:st) =
              if elem '$' (first pFst st) then upPredict pNxt o (sort (addAll (tkPredict pNxt o) (addAll (addWithout (first pFst st) "") (tkPredict pNxt n))))
                     else if st == [] then upPredict pNxt o (sort (addAll (tkPredict pNxt o) (tkPredict pNxt n)))
                            else upPredict pNxt o (sort (addWithout (tkPredict pNxt o) (first pFst st)))

crtNxt :: Grammar -> Predict
crtNxt gr = 
    let fstnotTerm = fst (head gr)
        grData = fromGrammar gr
    in sort ([(p, "$") | (p, _) <- grData, p == fstnotTerm] ++ [(pp, "") | (pp, _) <- (deleteBy (\_ x -> (fst x) == fstnotTerm) (' ',[""]) grData)])

---------------------Тестові дані ---------------------------
 
gr0, gr1, gr2, gr3, gr4, gr5:: Grammar
--  LL(1)-граматики
gr0 = [('S',"aAS"),('S',"b"), ('A',"a"), ('A',"bSA")]  
gr1 = [('S',"TV"),('T',"d"),('T',"(S)"),('V',"+TV"),('V',"-TV"),('V',"")]  
gr2 = [('E',"TU"),('U',""),('U',"+TU"),('U',"-TU"),
       ('T',"FV"),('V',""),('V',"*FV"),('V',"%FV"),('V',"/FV"),
       ('F',"d"),('F',"(E)")] 
-- не LL(1)-граматики
gr3 = [('S',"aAS"), ('S',"a"),('A',"SbA"),('A',"ba"),('S',"")]
gr4 = [('E',"E+T"),('E',"T"), ('T',"T*F"), ('T',"F"), ('F',"d"),('F',"(E)") ]   
gr5 = [('E',"E+T"), ('E',"E-T"),('E',"T"), 
       ('T',"T*F"), ('T',"T%F"), ('T',"T/F"), ('T',"F"), 
       ('F',"d"),('F',"(E)") ]

-- прогнозуючі таблиці початкових терміналів Fst
pFst0, pFst1, pFst2, pFst3, pFst4, pFst5 :: Predict
pFst0 = [('A',"ab"),('S',"ab")]
pFst1 = [('S',"(d"),('T',"(d"),('V',"$+-")]
pFst2 = [('E',"(d"),('F',"(d"),('T',"(d"),('U',"$+-"),('V',"$%*/")]
pFst3 = [('A',"ab"),('S',"$a")]
pFst4 = [('E',"(d"),('F',"(d"),('T',"(d")]
pFst5 = [('E',"(d"),('F',"(d"),('T',"(d")]

-- прогнозуючі таблиці наступних терміналів Nxt
pNxt0, pNxt1, pNxt2, pNxt3, pNxt4, pNxt5 :: Predict
pNxt0 = [('A',"ab"),('S',"$ab")]
pNxt1 = [('S',"$)"),('T',"$)+-"),('V',"$)")]
pNxt2 = [('E',"$)"),('F',"$%)*+-/"),('T',"$)+-"),('U',"$)"),('V',"$)+-")]
pNxt3 = [('A',"$ab"),('S',"$b")]
pNxt4 = [('E',"$)+"),('F',"$)*+"),('T',"$)*+")]
pNxt5 = [('E',"$)+-"),('F',"$%)*+-/"),('T',"$%)*+-/")]   

-- управляючі таблиці 
ctl0, ctl1, ctl2 :: Control 
ctl0 = [(('A','a'),2),(('A','b'),3),(('S','a'),0),(('S','b'),1)]
ctl1 = [(('S','('),0),(('S','d'),0),(('T','('),2),(('T','d'),1),
        (('V','$'),5),(('V',')'),5),(('V','+'),3),(('V','-'),4)]
ctl2 = [(('E','('),0),(('E','d'),0),(('F','('),10),(('F','d'),9),
        (('T','('),4),(('T','d'),4),(('U','$'),1),(('U',')'),1),
        (('U','+'),2),(('U','-'),3),(('V','$'),5),(('V','%'),7),
        (('V',')'),5),(('V','*'),6),(('V','+'),5),(('V','-'),5),(('V','/'),8)]
 