module Main where
data MyType a
    = MyBool Bool | MyAnything a
    | MyString String

-- data BoolTest 
    -- = 

var1 :: MyType Int -> String
-- var1 :: MyType -> BoolTest -> String
var1 (MyBool a) = show a 
var1 (MyString a)  = show a
var1 ( MyAnything a) = show a

data MyTuple a b = MyTuple a b deriving Show 


myswap :: MyTuple a b -> MyTuple b a 
myswap (MyTuple a b) = MyTuple b a


main :: IO ()
main = putStrLn (var1 (MyBool True))

