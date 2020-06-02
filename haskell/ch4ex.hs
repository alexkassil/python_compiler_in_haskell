-- 1. safe standard partial list functions
safeHead :: [a] -> Maybe a
safeTail :: [a] -> Maybe [a]
safeLast :: [a] -> Maybe a
--safeInit :: [a] -> Maybe [a]--}

safeHead []     = Nothing
safeHead (x:_)  = Just x

safeTail []     = Nothing
safeTail (_:xs) = Just xs

safeLast []     = Nothing
safeLast [x]    = Just x
safeLast (_:xs) = safeLast xs

tempInit [x]    = []
tempInit (x:xs) = x : (tempInit xs)

safeInit []     = Nothing
safeInit xs = Just (tempInit xs)

-- 2. Split on where predicate is false
splitWith :: (a -> Bool) -> [a] -> [[a]]

splitWith _ [] = []
splitWith p xs =
  let (pre, suf) = break p xs
  in if null pre
        then ((splitWith p) . (dropWhile p)) suf
        else pre : ((splitWith p) . (dropWhile p)) suf
