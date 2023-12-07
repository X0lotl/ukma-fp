{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use =<<" #-}
module Khomichenko13 where

import Data.Char (isDigit, digitToInt)

data Term   =  Nmb Int         -- десяткове число без знаку
            | Var String       -- змінна, довільний ідентифікатор
            | App Term Term    -- операція застосування
            | Abs String Term  --  операція абстракції
           deriving (Show, Eq)
type Contex = [(String,Term)]

-- Задача 1.a -----------------------------------------
addVar :: String -> [String] -> [String]
addVar var vars
  | var `elem` vars = vars
  | otherwise       = var : vars

-- Задача 1.b ----------------------------------------- 
delVar :: String -> [String] -> [String]
delVar var = filter (/= var)

-- Задача 1.c -----------------------------------------
unionV :: [String] -> [String] -> [String]
unionV xs ys = xs ++ filter (`notElem` xs) ys

-- Задача 1.d ----------------------------------------- 

freeVars :: Term -> [String]
freeVars (Nmb _) = []
freeVars (Var x) = [x]
freeVars (App x y) = unionV (freeVars x) (freeVars y)
freeVars (Abs x t) = delVar x (freeVars t)

-- Задача 2.a -----------------------------------------
deleteSyn :: String -> Contex -> Contex
deleteSyn key =
        filter (\(contextKey, _) -> key /= contextKey)

-- Задача 2.b -----------------------------------------
iswfTerm :: Term -> Contex -> Bool
iswfTerm term ctx =
        all (`elem` map fst ctx) (freeVars term)

-- Задача 2.c -----------------------------------------
iswfContex :: Contex -> Bool
iswfContex ctx = all (\((_, ctxElemTerm), ctxElemId) -> iswfTerm ctxElemTerm (take ctxElemId ctx)) (zip ctx [0..(length ctx - 1)])

-- Задача 3.a -----------------------------------------
isNumberInnerTerm :: Term -> String -> String -> Bool
isNumberInnerTerm (Nmb _) _ _ = False
isNumberInnerTerm (Var x) _ var2 =
    x == var2
isNumberInnerTerm (App (Var x) t) var1 var2 =
    x == var1 && isNumberInnerTerm t var1 var2
isNumberInnerTerm (App _ _) _ _ = False
isNumberInnerTerm (Abs _ _) _ _ = False

isNumber :: Term -> Bool
isNumber (Nmb _) = True
isNumber (Var _) = False
isNumber (App _ _) = False
isNumber (Abs var1 (Abs var2 t)) = isNumberInnerTerm t var1 var2
isNumber (Abs _ _) = False

-- Задача 3.b -----------------------------------------
inNumberInnerTerm :: Term -> Int
inNumberInnerTerm (Var _) = 0
inNumberInnerTerm (App _ t) = 1 + inNumberInnerTerm t
inNumberInnerTerm _ = undefined

getNumberTermStructure :: Term -> Term
getNumberTermStructure (Abs _ (Abs _ t)) = t
getNumberTermStructure _ = undefined

inNumber :: Term -> Term
inNumber t =
  if isNumber t
    then case t of
           Nmb x -> Nmb x
           _     -> Nmb (inNumberInnerTerm (getNumberTermStructure t))
    else undefined

-- Задача 3.c -----------------------------------------
compress :: Term -> Term
compress (Nmb x) = Nmb x
compress (Var x) = Var x
compress (App x y) = App (compress x) (compress y)
compress (Abs x y) =
  if isNumber (Abs x y) then
    inNumber (Abs x y)
  else Abs x (compress y)


-- Задача 4 -----------------------------------------
reduce :: Term -> String -> Term -> Term
reduce (Var x) varN t
  | x == varN = t
  | otherwise = Var x
reduce (Nmb x) _ _ = Nmb x
reduce (App x y) varN t = App (reduce x varN t) (reduce y varN t)
reduce (Abs x y) varN t =
  Abs
    (newVar (freeVars y) newX)
    (reduce (reduce y newX (Var (newVar (freeVars y) newX))) varN t)
  where
    newX = newVar (freeVars y) x

-- Задача 5 -----------------------------------------
evalStep :: Term -> Contex -> Maybe Term
evalStep (Nmb x) _ = Just (integerTerm x)
evalStep (Var x) ctx =
  maybe Nothing Just (lookup x ctx)
evalStep (App (Abs x t1) t2) _ = Just (reduce t1 x t2)
evalStep (App t1 t2) ctx =
  case evalStep t1 ctx of
    Just t1' -> Just (App t1' t2)
    Nothing -> fmap (\t2' -> App t1 t2') (evalStep t2 ctx)
evalStep (Abs x t) ctx = fmap (Abs x) (evalStep t ctx)

-- Задача 6 -----------------------------------------
eval :: Int -> Term -> Contex -> Maybe Term
eval 0 _ _ = Nothing
eval n t ctx =
  case evalStep t ctx of
    Just t' -> eval (n - 1) t' ctx
    Nothing -> Just (compress t)

-- Задача 7 -----------------------------------------
normalizeString :: String -> String
normalizeString [] = []
normalizeString (' ':' ':str) = normalizeString (' ':str)
normalizeString (' ':')':str) = normalizeString (')':str)
normalizeString (' ':'.':str) = normalizeString ('.':str)
normalizeString ('.':' ':str) = normalizeString ('.':str)
normalizeString (x:str) = x : normalizeString str

parseFirstVarName :: String -> String
parseFirstVarName [] = []
parseFirstVarName str =
  if head str /= ' ' && head str /= '.'
    then head str : parseFirstVarName (tail str)
  else []

parseVarTerm :: String -> Term
parseVarTerm str = Var (parseFirstVarName str)

intLength :: Int -> Int
intLength val = go val 0
  where
    go 0 count = count
    go val count = go (val `div` 10) (count + 1)

parseNextDigit :: String -> Maybe Int
parseNextDigit [] = Nothing
parseNextDigit (x:xs)
  | isDigit x = Just (foldl (\acc d -> acc * 10 + digitToInt d) (digitToInt x) xs)
  | otherwise = Nothing

parseNextDigitTerm :: String -> Maybe Term
parseNextDigitTerm str =
  maybe Nothing (\x -> Just (Nmb x)) (parseNextDigit str)

findIndex' :: Eq a => [a] -> a -> Int -> Maybe Int
findIndex' [] _ _ = Nothing
findIndex' list element currentIndex =
  if head list == element
    then Just currentIndex
  else findIndex' (tail list) element (currentIndex + 1)

findIndex :: Eq a => [a] -> a -> Maybe Int
findIndex list element = findIndex' list element 0

findNextBracket' :: String -> Int -> Maybe Int
findNextBracket' [] _ = Nothing

findNextBracket' ")" 0 = Just 0
findNextBracket' (')':_) 0 = Just 0

findNextBracket' ('(':str) depth = 
  maybe Nothing (\x -> Just (x + 1)) (findNextBracket' str (depth + 1))

findNextBracket' (')':str) depth = 
  maybe Nothing (\x -> Just (x + 1)) (findNextBracket' str (depth - 1))

findNextBracket' str depth = 
  maybe Nothing (\x -> Just (x + 1)) (findNextBracket' (drop 1 str) depth)


findNextBracket :: String -> Maybe Int
findNextBracket str = findNextBracket' str 0

splitBy :: Eq a => [a] -> a -> [[a]]
splitBy [] _ = []
splitBy list element =
  maybe [list] (\index ->
    take index list : splitBy (drop (index + 1) list) element
  ) (findIndex list element)

trimStart :: String -> String
trimStart [] = []
trimStart str =
  if head str == ' ' then
    trimStart (tail str)
  else str

trimEnd :: String -> String
trimEnd str = reverse (trimStart (reverse str))

trim :: String -> String
trim = trimStart . trimEnd

joinWith :: [[a]] -> a -> [a]
joinWith list separator =
  foldl (\left right -> left ++ [separator] ++ right) [] list

dropTrailingBrackets' :: String -> String
dropTrailingBrackets' (')':str) = dropTrailingBrackets' str
dropTrailingBrackets' str = str

dropTrailingBrackets :: String -> String
dropTrailingBrackets str = reverse (dropTrailingBrackets' (reverse str))

findNextApplicationIndex :: String -> Maybe Int
findNextApplicationIndex ('(':rest) = 
  maybe Nothing (\nextInnerAdjacentBracketIndex -> 
    let afterBracketStr = drop (nextInnerAdjacentBracketIndex + 1) rest in
      if null afterBracketStr then Nothing
      else if head afterBracketStr == ' ' then
        Just (nextInnerAdjacentBracketIndex + 2)
      else Nothing
  ) (findNextBracket rest)

findNextApplicationIndex str =
  maybe Nothing (\nextSpaceIndex ->
    maybe (Just nextSpaceIndex) (\nextAdjacentBracketIndex ->
      if nextSpaceIndex < nextAdjacentBracketIndex then 
        Just nextSpaceIndex
      else Nothing
    ) (findNextBracket str)
  ) (findIndex str ' ')

findRightestApplicationIndex :: String -> Maybe Int
findRightestApplicationIndex str =
  maybe Nothing (\nextApplicationIndex -> 
    maybe (Just nextApplicationIndex) (\nextRightestApplicationIndex -> 
      Just (nextApplicationIndex + 1 + nextRightestApplicationIndex)
    ) (findRightestApplicationIndex (drop (nextApplicationIndex + 1) str))
  ) (findNextApplicationIndex str)

parseApplication :: String -> Maybe Term
parseApplication str = 
  maybe Nothing (\rightestApplicationIndex -> 
    maybe Nothing (\rightApplicationTerm -> 
      maybe Nothing (\leftApplicationTerm -> 
        Just (
          App leftApplicationTerm rightApplicationTerm
        )
      ) (parseTerm (take rightestApplicationIndex str))
    ) (parseTerm (drop (rightestApplicationIndex + 1) str))
  ) (findRightestApplicationIndex str)

parseTerm :: String -> Maybe Term
parseTerm ('\\' : rest) = 
  let maybeNextTermIndex = findIndex rest '.' in
    case maybeNextTermIndex of
      Nothing -> Nothing
      Just nextTermIndex ->
        let varNames = filter (not . null) (splitBy (take nextTermIndex rest) ' ') in
          if null varNames then
            Nothing
          else if null (tail varNames) then
            maybe Nothing (
              \parsedNextTerm ->
                Just (
                  Abs 
                  (head varNames)
                  parsedNextTerm
                )
            ) (parseTerm (drop (nextTermIndex + 1) rest))
          else
            maybe 
              Nothing 
              (\x -> 
                Just (
                  Abs 
                  (head varNames) 
                  x
                )
              ) 
              (parseTerm 
                ('\\' : 
                  (
                    (
                      joinWith 
                      (tail varNames) 
                      ' '
                    ) 
                    ++ 
                    (
                      drop 
                      (nextTermIndex) 
                      rest
                    )
                  )
                )
              )

parseTerm (')':x) = parseTerm x

parseTerm x = 
  let normalizedText = normalizeString (trim x) in
    maybe (
      if (not (null normalizedText)) && head normalizedText == '(' then 
        parseTerm (tail normalizedText)
      else
      if null normalizedText then Nothing
      else if head normalizedText == '\\' then parseTerm normalizedText
      else if isDigit (head normalizedText) then
        parseNextDigitTerm (dropTrailingBrackets normalizedText)
      else
        Just (parseVarTerm (dropTrailingBrackets normalizedText))
    ) 
    (\_ ->
      maybe Nothing Just (parseApplication normalizedText)
    ) 
    (findNextApplicationIndex normalizedText)

--------------------------------------------------------
-- integerTerm - з числа в вираз
integerTerm :: Int -> Term
integerTerm n  = (Abs "s" (Abs "z" (buildTerm n)))
  where buildTerm 0 = Var "z"
        buildTerm j = (App (Var "s") (buildTerm (j-1)))

--  New Name -- якщо імя dddname, де ddd-цифри і n-буква, то початкове імя - name 
-- якщо змінна ccc, то її нові імена 0ccc,...,9ccc,09ccc,...
-- цифри на початку - це створення нового імені (problem name capture)
newVar :: [String] -> String -> String
newVar fvs nm = (until (\n -> notElem n fvs) next) (next nm)   -- flip elem fvs
  where next n@(c:_)| c=='9'    = '0':n
        next (c:cx) | isDigit c = (succ c):cx
        next n      = '0':n

--------------------------------------------------------
-- Тестові приклади
term0, term0a, term1, term1a, term1b, term1c :: Term
-- \s,z -> s (s z)
term0 = Abs "s" (Abs "z" (App (Var "s") (App (Var "s") (Var "z"))))
term0a = Abs "z" (App (Var "s") (App (Var "s") (Var "z")))
term1 = Abs "y" (App (Abs "x" (Abs "y" (App (Var "x") (Var "y")))) (Var "y"))
term1a = App (Abs "x" (Abs "y" (App (Var "x") (Var "y")))) (Var "y")
term1b = Abs "x" (Abs "y" (App (Var "x") (Var "y")))
term1c = Abs "y" (App (Var "x") (Var "y"))

term2, term2a, termAnd, termTest :: Term
term2 = App (App (Abs "f" (Abs "g" (Abs "x" (App (App (Var "f") (Var "x")) (App (Var "g") (Var "x"))))))
                 (Abs "x" (Abs "y" (Var "x")))
            )
            (Abs "x" (Abs "y" (Var "x")))
term2a = App (Var "x") (App (Abs "x" (Var "x")) (App (Abs "x" (Var "x")) (Var "z")))
termAnd = Abs "x" (Abs "y" (App (App  (Var "x") (Var "y")) (Var "false")))
termTest = Abs "x" (Abs "x" (Abs "y" (Var "y")))

cont1 :: Contex
cont1 = [("true",Abs "x" (Abs "y" (Var "x")))
        ,("false",Abs "x" (Abs "y" (Var "y")))
        ,("test",Abs "l" (Abs "m" (Abs "n" (App (App (Var "l") (Var "m")) (Var "n")))))
        ,("iszero",Abs "n" (App (App (Var "n") (Abs "x" (Var "false"))) (Var "true")))
        ,("plus",Abs "m" (Abs "n" (Abs "f" (Abs "x" (App (App (Var "n") (Var "f")) (App (App (Var "m") (Var "f")) (Var "x")))))))
        ,("mult",Abs "m" (Abs "n" (Abs "f" (App (Var "m") (App (Var "n") (Var "f"))))))
        ,("pred",Abs "n" (Abs "f" (Abs "x" (App (App (App (Var "n") (Abs "g" (Abs "h" (App (Var "h") (App (Var "g") (Var "f")))))) (Abs "u" (Var "x"))) (Abs "x" (Var "x"))))))
        ,("fixM",Abs "f" (App (Abs "x" (App (Var "f") (Abs "y" (App (App (Var "x") (Var "x")) (Var "y"))))) (Abs "x" (App (Var "f") (Abs "y" (App (App (Var "x") (Var "x")) (Var "y")))))))
        ,("sumR",Abs "r" (Abs "n" (App (App (App (Var "test") (App (Var "iszero") (Var "n"))) (Nmb 0)) (App (App (Var "plus") (Var "n")) (App (Var "r") (App (Var "pred") (Var "n")))))))
        ,("factR",Abs "fact" (Abs "n" (App (App (App (Var "test") (App (Var "iszero") (Var "n"))) (Nmb 1)) (App (App (Var "mult") (Var "n")) (App (Var "fact") (App (Var "pred") (Var "n")))))))
        ,("sum",App (Var "fixM") (Var "sumR"))
        ,("factor",App (Var "fixM") (Var "factR"))
        ]

termS2 :: String
termS2 = "(\\f g x. f x (g x))   (\\x y .x) (\\x y .x)"