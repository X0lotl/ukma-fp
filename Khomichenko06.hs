{-# OPTIONS_GHC -Wall #-}
module Khomichenko06 where

import Data.Char(isDigit, digitToInt)

-- Задача 1 -----------------------------------------
analyseG :: String -> Bool 
analyseG [] = False
analyseG w = case s w of
  Just rest -> case a rest of
    Just [] -> False
    _ -> True
  _ -> True

s :: String -> Maybe String
s ('a':rest) = case s rest of
  Just rest' -> case match 'b' (Just rest') of
    Just rest'' -> case a rest'' of
      Just rest''' -> match 'a' (Just rest''')
      _ -> Nothing
    _ -> Nothing
  _ -> Nothing
s ('b':rest) = Just rest
s _ = Nothing

a :: String -> Maybe String
a ('b':'a':rest) = case a rest of
    Just rest' -> case match 'S' (Just rest') of
      Just rest'' -> s rest''
      _ -> Nothing
    _ -> Nothing
a ('a':rest) = Just rest
a _ = Nothing

-- Задача 2 ----------------------------------------
balance :: String -> Bool
balance = undefined

b :: String -> Maybe String 
b = undefined

-- Задача 3 -----------------------------------------
analyseExpr :: String -> Bool 
analyseExpr = undefined

ae :: String -> Maybe String 
ae = undefined

aa :: String -> Maybe String 
aa = undefined

af :: String -> Maybe String 
af = undefined

-- Задача 4 -----------------------------------------
evalLeft :: String -> Maybe Int 
evalLeft = undefined

le :: String -> Maybe String 
le = undefined

la :: (Int,String) -> Maybe (Int,String) 
la = undefined

lf :: String -> Maybe String 
lf = undefined

-- Задача 5 -----------------------------------------
evalRigth :: String -> Maybe Int 
evalRigth = undefined

re :: String -> Maybe String 
re = undefined

ra :: (Int,String) -> Maybe (Int,String) 
ra = undefined

rf :: String -> Maybe String 
rf = undefined

-- Задача 6 -----------------------------------------
evalPrior :: String -> Maybe Int 
evalPrior = undefined

pe :: String -> Maybe String 
pe = undefined

pa :: (Int,String) -> Maybe (Int,String) 
pa = undefined

pt :: String -> Maybe String 
pt = undefined

pb :: (Int,String) -> Maybe (Int,String) 
pb = undefined

pf :: String -> Maybe String 
pf = undefined

------------------------------------------------------
match :: Char -> Maybe String -> Maybe String 
match c1 (Just (t:st)) | c1==t = Just st
match _ _                      = Nothing 

inOp :: Char -> Int -> Int -> Int
inOp c2 = case c2 of {'+' -> (+); '-' -> (-); '*' -> (*); _ -> undefined}