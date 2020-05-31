import Debug.Trace
-- 1. Write a function that computes the number of elements in a list
myLength :: [a] -> Int
myLength (_:xs) = 1 + myLength xs
myLength _      = 0

testLength a = if (length a) /= (myLength a)
                  then error ("lengths differ for " ++ (show a))
                  else "Success"



-- 3. Write a function that computes the mean of a list
--mean :: (Num a) => [a] -> a
--mean :: (Fractional a, Num b) => [b] -> a
mean xs | xs /= [] = (fromIntegral (sum xs)) / (fromIntegral (myLength xs))
mean [] = 0.0

testMean (a, expected) = if (mean a) /= expected
                  then error ("means differ. Expected: " ++ (show expected) ++ "\nBut got " ++ (show (mean a)))
                  else "Success"

-- 4. Write a function that turns a list into a palindrome

toPalindrome :: [a] -> [a]
toPalindrome a = a ++ reverse a

testToPalindrome (a, expected) = if toPalindrome a /= expected
                                    then error ("for input: " ++ show a
                                                ++ "\nExpected: " ++ show expected
                                                ++ "\nBut got: " ++ show (toPalindrome a))
                                    else "Success"

-- 5. Write a function that checks if a list is a palindrome

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [x]    = True
isPalindrome []     = True
isPalindrome (x:xs) = isPalindrome (init xs) && x == (last xs)
testIsPalindromeTrue a = if isPalindrome a
                            then "Success"
                            else error ("should be True for input: " ++ show a)
testIsPalindromeFalse a =  if (not (isPalindrome a))
                            then "Sucess"
                            else error ("should be False for input: " ++ show a)

main = let lists = [[1],[1, 2],[1, 2, 3],[1..100],[]]
       in do
        print ( map testLength lists)
        print ( map testMean [([1], 1.0), ([1,2],1.5), ([1, 2, 3], 2.0), ([1..100], 50.5), ([], 0)])
        print ( map testToPalindrome [ ([1], [1, 1]), ([1, 2, 3], [1, 2, 3, 3, 2,1]), ([], []) ])
        print ( map testIsPalindromeTrue ((map toPalindrome lists) ++ [[1], [1, 2, 1], [1, 2, 3, 4, 3, 2, 1]]) )
        print ( map testIsPalindromeFalse [[1, 2], [2, 1], [1, 2, 3], [1, 2, 3, 4, 2, 1] ] )

