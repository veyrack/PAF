{-# LANGUAGE OverloadedStrings #-}
module Stat where

import Data.Text (Text)
import Data.Text (unpack)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List



compteCarac :: String -> Int -> Int
compteCarac [] n = n
compteCarac (t:q) n = (compteCarac q (n+(carac t)))

carac :: Char -> Int
carac '\n' = 0
carac _ = 1 

compteMot :: String -> Int -> Int
compteMot [] n = n
compteMot (t:q) n = (compteMot q (n+(mot t)))

mot :: Char -> Int
mot ' ' = 1
mot '\t' = 1
mot '\n' = 1
mot _ = 0 


compte :: Text -> Int
compte text = let s = unpack text in 
    compteCarac s 0

compteMotF :: Text -> Int
compteMotF text = let s = unpack text in 
    case s of
        [] -> 0
        _ -> compteMot s 1 

addDic :: String -> (Map String Int) -> (Map String Int)
addDic m dic = if Map.null dic 
    then (Map.fromList [(m,1)])
    else Map.insert m (if (Map.lookup m dic == Nothing) 
        then 1
        else (dic Map.! m) +1) dic


consDict :: Text -> (Map String Int)
consDict text = let s = unpack text in
    let l = words s in
        foldl (\acc li  -> (addDic li acc)) Map.empty l



imprimeDict :: (Map String Int) -> [Char]
imprimeDict dic = foldl (\acc s -> acc ++ show s ++ "\n") "" (Map.toList dic)


sortGT :: (String,Int) -> (String,Int) -> Ordering
sortGT (a1, b1) (a2, b2)
    | b1 < b2 = GT
    | b1 > b2 = LT
    | b1 == b2 = compare a1 a2

triDict :: (Map String Int) -> (Map String Int)
triDict dic = Map.fromList (Data.List.sortBy sortGT (Map.toList dic)) --Marche pas