{-# LANGUAGE DeriveAnyClass #-}
class  Eqq a  where  
    (===), (/==) :: a -> a -> Bool  
 
        -- Minimal complete definition:  
        --      (===) or (/==)  
    x /== y     =  not (x === y)  
    x === y     =  not (x /== y)

data MyBool = MyTrue | MyFalse
            deriving (Show)

instance Eqq MyBool where
  (===) MyTrue  MyTrue  = True
  (===) MyFalse MyFalse = True
  (===) _       _       = False
