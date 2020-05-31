-- file: add.hs
add a b = a + b

sumList (x:xs) = x + sumList xs
sumList []     = 0

reduce f (x:xs) s = f x (reduce f (xs) s)
reduce f  []        s = s

mymap f (x:xs) = [(f x)] ++ mymap f xs
mymap f []      = []

myfilter f (x:xs) = if (f x)
                        then [x] ++ myfilter f xs
                        else myfilter f xs
myfilter f []      = []
x = reduce (+) (myfilter (\x -> x `mod` 2 == 0) (mymap (\x -> x * x) [1..10])) 0
