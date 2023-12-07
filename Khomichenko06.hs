{-# OPTIONS_GHC -Wall #-}
module Khomichenko06 where

import Data.Char(isDigit, digitToInt)

-- Задача 1 -----------------------------------------
analyseG :: String -> Bool 
analyseG st1 = case s st1 of
    Just st2 -> null st2
    Nothing -> False 

s :: String -> Maybe String
s ('a':st1) = case s st1 of
    Just ('b':st2) -> case a st2 of
        Just ('a':st3) -> Just st3
        _              -> Nothing
    _              -> Nothing

s ('b':st1) = Just st1

a :: String -> Maybe String
a ('b':'a':st1)= case a st1 of 
    Just st2 -> s st2 
    _ -> Nothing

a('a': st1) = Just st1
   
-- Задача 2 ----------------------------------------
balance :: String -> Bool
balance st = case b st of 
    Just st1 -> null st1
    Nothing -> False 

b :: String -> Maybe String 
b ('(': st1)= case b st1 of      
      Just (')':st2) -> b st2 
      _                -> Nothing  
b ('[': st1)= case b st1 of      
      Just (']':st2) -> b st2 
      _                -> Nothing  

b ('{': st1)= case b st1 of      
      Just ('}':st2) -> b st2 
      _                -> Nothing  
b st        = Just st

-- Задача 3 -----------------------------------------
analyseExpr :: String -> Bool 
analyseExpr st1 = case ae st1 of 
   Just st2 -> null st2  
   _          -> False 


ae :: String -> Maybe String 
ae st1 = case af st1 of 
     Just st2 -> aa st2 
     Nothing  -> Nothing  


aa :: String -> Maybe String 
aa (p:st1) | elem p "+*-" 
             = case af st1 of  
      Just st2 -> aa st2  
      Nothing  -> Nothing 
aa st1      = Just st1  


af :: String -> Maybe String 
af (d:st1) | elem d "1234567890" 
                = Just st1 
af _            = Nothing


-- Задача 4 -----------------------------------------
evalLeft :: String -> Maybe Int 
evalLeft st1 = case le st1 of 
   Just (v,st2)| null st2 -> Just v 
   _                         -> Nothing 


le :: String -> Maybe (Int, String) 
le st1 =  case lf st1 of 
     Just (v1,st2) -> la (v1,st2) 
     Nothing       -> Nothing 

la :: (Int,String) -> Maybe (Int,String)
la (v1, (d:st1))
  | elem d "+*-" = case lf st1 of
      Just (v2, st2) -> 
        let v = if d == '+' then v1 + v2 
                else if d == '*' then v1 * v2 
                else v1 - v2 
        in la (v, st2)
      Nothing -> Nothing

la (v1, st1) = Just (v1, st1)


lf :: String -> Maybe (Int, String) 
lf ('(':st1) = case le st1 of 
                  Just (v, (')':st2)) -> Just (v, st2) 
                  _                -> Nothing  
lf (d:st1) 
  | elem d "1234567890" = Just (digitToInt d, st1) 
  | otherwise          = Nothing

lf _  = Nothing


-- Задача 5 -----------------------------------------
evalRight :: String -> Maybe Int 
evalRight st1 = case re (reverse st1) of 
   Just (v, st2) | null st2 -> Just v 
   _                         -> Nothing

re :: String -> Maybe (Int, String) 
re st1 = case rf st1 of 
     Just (v1, st2) -> ra (v1, st2) 
     Nothing       -> Nothing 

ra :: (Int, String) -> Maybe (Int, String) 
ra (v1, (d:st1))
  | elem d "+*-" = case rf st1 of
      Just (v2, st2) -> 
        let v = if d == '+' then v2 + v1 
                else if d == '*' then v2 * v1 
                else v2 - v1 
        in ra (v, st2)
      Nothing -> Nothing
ra (v1, st1) = Just (v1, st1)

rf :: String -> Maybe (Int, String) 
rf (')':st1) = case re st1 of 
                  Just (v, ('(':st2)) -> Just (v, st2) 
                  _                -> Nothing  
rf (d:st1) 
  | elem d "1234567890" = Just (digitToInt d, st1) 
  | otherwise          = Nothing

rf _  = Nothing

-- Задача 6 -----------------------------------------
evalPrior :: String -> Maybe Int 
evalPrior st1 = case pe st1 of
  Just (v, st2) | null st2 -> Just v
  _                        -> Nothing

pe :: String -> Maybe (Int, String)
pe st1 = case pt st1 of
    Just (v1, st2) -> pa (v1, st2)
    Nothing        -> Nothing

pa :: (Int, String) -> Maybe (Int, String) 
pa (v1, (d:st1)) | elem d "+-" = case pt st1 of
      Just (v2, st2) ->
          let v = if d == '+' then v1 + v2 else v1 - v2
          in pa (v, st2)
      Nothing -> Nothing
pa (v1, st1) = Just (v1, st1)

pt :: String -> Maybe (Int, String)
pt st1 = case pf st1 of
        Just (v, st2) -> pb (v, st2)
        _             -> Nothing 

pb :: (Int, String) -> Maybe (Int, String) 
pb (v1, (d:st1)) | d == '*' = case pf st1 of 
            Just (v2, st2) -> let v = v1 * v2 in pb (v, st2)
            Nothing        -> Nothing 
pb (v1, st1) = Just (v1, st1)

pf :: String -> Maybe (Int, String) 
pf ('(':st1) = case pe st1 of
        Just (v, (')':st2)) -> Just (v, st2)
        _                   -> Nothing 
pf (d:st1) 
  | elem d "1234567890" = Just (digitToInt d, st1) 
  | otherwise          = Nothing
pf _ = Nothing


------------------------------------------------------
match :: Char -> Maybe String -> Maybe String 
match c1 (Just (t:st)) | c1==t = Just st
match _ _                      = Nothing 

inOp :: Char -> Int -> Int -> Int
inOp c2 = case c2 of {'+' -> (+); '-' -> (-); '*' -> (*); _ -> undefined}