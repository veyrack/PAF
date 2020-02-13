{-# LANGUAGE OverloadedStrings #-}
module Stat where

import Data.Text (Text)
import Data.Text (unpack)



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
        [] -> compteMot s 0
        _ -> compteMot s 1 

consDict :: Text -> Text
consDict s = s

imprimeDict :: Text -> Text
imprimeDict s = s

triDict :: Text -> Text
triDict s = s