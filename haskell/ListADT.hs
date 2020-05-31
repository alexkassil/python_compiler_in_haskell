data List a = Cons a (List a)
            | Nil
              deriving (Show)

myList = Cons 1 (Cons 2 (Cons 3 Nil))



fromList (x:xs) = Cons x (fromList xs)
fromList []     = Nil

toList (Cons x xs) = x:toList xs
toList Nil         = []

car :: List a -> a
car Nil = error "list is nil"
car (Cons x xs) = x

cdr :: List a -> List a
cdr Nil = error "list is nil"
cdr (Cons x xs) = xs

