module Main where
import Data.Char  
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
-- type Age = Int
-- type Title = String

-- data Person = Person Int String deriving (Show)

-- fred = Person 21 "freddy"

-- personName ( Person age name) = name
-- personAge :: Person -> String
-- personAge  (Person age title ) = age
-- personName  (Person age title ) = title

-- data Person2 = Person2 {
--     personName2 :: PersonName2,
--     personAge2 :: PersonAge2,
-- } deriving ( Show )

data Optional a 
    = None 
    | Some a deriving Show

parseOpenParen :: String -> (String, Optional Char)
parseOpenParen "" = ("", None)
parseOpenParen (c : cs) = 
  if c == '('
    then (cs, Some '(') 
    else (c : cs, None)


parseCloseParen :: String -> (String, Optional Char)
parseCloseParen "" = ("", None)
parseCloseParen (c : cs) = 
  if c == ')'
    then (cs, Some ')') 
    else (c : cs, None)


data Final a = Full a deriving Show


parseAlpha :: String -> ([Char], Optional Char)
parseAlpha "" = ("", None)
parseAlpha (c: cs) =
  if isAlpha c
    then (cs, Some c)
    else (c: cs, None)

-- parseMultiAlpha :: String -> Final String
-- parseMultiAlpha "" str = ""
-- parseMultiAlpha (c: cs) str =
--   if isAlpha c
--     then parseMultiAlpha cs c
--     else Full str

-- data MyShow a
--   = ToShow a deriving Show

-- append :: [Char] -> String -> MyShow [Char] 
-- append a xs = MyShow (xs ++ a )

-- parseMulti :: String -> [Char] -> MyShow [Char]
-- parseMulti "" str = ToShow ""
-- parseMulti (c:cs) str = 
--   if isAlpha c  
--     then do
--       append c str
--       parseMulti cs str
--     else ToShow str
  

-- newParseAlpha :: String -> ( String, Optional Char)


-- parse :: String -> (String, Optional String)
-- parse str =
--   case parseOpenParen str of 
--     ( _, None) -> ( str, None)
--     ( str', Some c1) -> 
--       case parseAlpha str' of 
--         ( _, None) -> ( str', None)
--         ( str'', Some c2) -> 
--           case parseCloseParen str'' of 
--             ( _, None) -> ( str'', None)
--             ( str''', Some c3) -> ( str''', Some [c1,c2, c3 ])

-- parse2 :: String -> (String, Optional String)
-- parse2 str =
--   case parseOpenParen str of 
--     ( _, None) -> ( str, None)
--     ( str', Some c1) -> 
--       case parseAlpha str' of 
--         ( _, None) -> ( str', None)
--         ( str'', Some c2) -> 
--           case parseCloseParen str'' of 
--             ( _, None) -> ( str'', None)
--             ( str''', Some c3) -> ( str''', Some [c1,c2, c3 ])


class Put a where
  put :: a -> String


-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b
    --  ($) :: (a -> b) -> f a -> f b 


instance Put () where
  put () = "()"

instance Put Char where
  put x = [x]



instance (Put a, Put b) => Put (a, b) where
  put (x, y) = "(" ++ put x ++ "," ++ put y ++ ")"

-- data Tree a 
--   = Tip
--   | Bin a (Tree a) (Tree a)

instance Put Bool where
  put True = "True"
  put False = "False" 

-- instance Put a => Put (Tree a) where 
--   put Tip = "Tip"
--   put (Bin x xs ys) = "(Bin " ++ put x ++ " " ++ put xs ++ " " ++ put ys ++ ")"

-- put (Bin 'a' (Bin 'z' Tip Tip) Tip) 

instance (Put a, Put b, Put c) => Put (a, b, c) where
  put (x, y, z) = "(" ++ put x ++ "," ++ put y ++ "," ++ put z ++ ")"

-- look up these with hoogle
-- class Functor
-- class Applicative
-- class Monad

-- Something :: Functor -> Applicative -> Monad



parseOnlyAlpha :: String -> [Char]
parseOnlyAlpha "" = ""
parseOnlyAlpha (c:cs) = if 
  isAlpha c 
  then parseOnlyAlpha cs
  else [c]





lastButOne :: [a] -> [a] 
lastButOne list  = if length list <= 2
                    then take 1 list
                    else lastButOne (tail list)

-- bmap :: ( a-> b) -> First a -> First b
-- bmap = fmap

-- class Functor f where
--   fmap :: ( a->b ) -> f a -> f b


data Maybe2 a 
  = Just2 a
  | Nothing2 deriving Show

instance Functor Maybe2 where
  fmap f (Just2 a ) = Just2 (f a)
  fmap f Nothing2 = Nothing2


data Tree a 
  = Tip a
  | Branch (Tree a) (Tree a) deriving Show

instance Functor Tree where 
  fmap f ( Tip a ) = Tip ( f a)
  fmap f  ( Branch left right) = Branch (fmap f left) ( fmap  f right)




instance Applicative Maybe2 where
  pure a = Just2 a
  Just2 f <*> j = fmap f j 
  Nothing2 <*> _ = Nothing2
  

-- instance Applicative Tree where
--   pure = Tree

main :: IO ()
main = print ( lastButOne [1, 2])


