{-- Alex Kassil's python interpreter --}
import Data.Char

main = do
  -- Read
  putStr ">>> "
  input <- getLine
  -- Eval
  let tokens = getTokens input
  let result = eval tokens
  -- Print
  print result
  -- Loop
  main

eval :: [String] -> Int
eval [x] = read x :: Int
eval (x:op:xs)
  | op == "+" = (read x :: Int) + eval xs
  | op == "-" = (read x :: Int) - eval xs

getTokens :: String -> [String]
getTokens "" = []
getTokens x = getToken x : getTokens (skipToken x)

getToken :: String -> String
getToken (x:xs)
  | isDigit x = [x]
  | x == '+' || x == '-' = [x]
  | x == ' ' = getToken xs
  | otherwise = error ("Unknown input: " ++ [x])

skipToken :: String -> String
skipToken (x:xs)
  | isDigit x = xs
  | x == '+' || x == '-' = xs
  | x == ' ' = skipToken xs
  | otherwise = error ("Unknown input: " ++ [x])

{--getTokens [x]
  | isDigit x = [[x]]
  | otherwise = error ("Unknown input: " ++ [x])
--}
