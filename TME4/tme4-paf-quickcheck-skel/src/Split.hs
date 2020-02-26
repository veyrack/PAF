module Split where

import Data.List

split :: Char -> String -> [String]
split c s = let tmp = dropWhile (/=c) s in
    takeWhile (/=c) s : if tmp == "" 
        then [] 
        else split c (tail tmp)


unsplit :: Char -> [String] -> String
unsplit c s =  tail (foldl (\acc x -> acc ++ [c] ++ x ) "" s)
-- solution plus simple : unsplit c = concat . Data.List.intersperse [c]

prop_split_unsplit :: Char -> String -> Bool
prop_split_unsplit c str = unsplit c (split c str) == str

