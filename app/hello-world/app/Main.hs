module Main where
data MyType a
    = MyBool Bool | MyAnything a
    | MyString String

data Authors = Names [String]
data Title = Name String


data BookInfo = Book Int String [String]
                deriving (Show)


-- var1 :: MyType Int -> String
-- -- var1 :: MyType -> BoolTest -> String
-- var1 (MyString a)  = show a
-- var1 ( MyAnything a) = show a

-- data MyTuple a b = MyTuple a b deriving Show 


-- myswap :: MyTuple a b -> MyTuple b a 
-- myswap (MyTuple a b) = MyTuple b a

lastButOne :: [a] -> [a] 
lastButOne list  = if length list <= 2
                    then take 1 list
                    else lastButOne (tail list)



main :: IO ()
main = print ( lastButOne [])


