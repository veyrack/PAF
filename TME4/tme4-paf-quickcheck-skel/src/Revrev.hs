module Revrev where

-- reverse :: [a] -> [a]

prop_revrev :: Eq a => [a] -> [a] -> Bool
prop_revrev xs ys = reverse (xs <> ys) == reverse ys <> reverse xs

