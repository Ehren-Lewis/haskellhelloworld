module Main where
-- data MyType a
--     = MyBool Bool | MyAnything a
--     | MyString String

-- data Authors = Names [String]
-- data Title = Name String


-- creating your own type
-- data BookInfo = Book Int String [String]
--                 deriving (Show)

-- creating a new value of type BookInfo
-- myBook = Book 9220 "The Lord of The Fallen" ["Fred Olsenberg", "Sirus Restwood"]


-- type synonym
-- used to make code more readable
type Age = Int
type Title = String

data Person = Person Int String deriving (Show)

-- fred = Person 21 "freddy"

-- personName ( Person age name) = name
personAge :: Perso -> String
personAge  (Person age title ) = age
personName  (Person age title ) = title


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
main = print ( lastButOne [1, 2])


