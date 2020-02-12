maxInt :: Integer -> Integer -> Integer
maxInt x y = 
    if x > y
        then x
        else y

fibo :: Int -> Int
fibo 0 = 1
fibo 1 = 1
fibo n = 
    if (n<0) 
        then -1
        else fibo (n-1) + fibo (n-2)