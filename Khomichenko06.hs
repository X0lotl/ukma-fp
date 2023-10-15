{-# OPTIONS_GHC -Wall #-}
module HWC06 where

import Data.Char(isDigit, digitToInt)

-- Задача 1 -----------------------------------------
analyseG :: String -> Bool 
analyseG = undefined

s :: String -> Maybe String
s = undefined

a :: String -> Maybe String
a = undefined
   
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