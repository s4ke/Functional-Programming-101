-- Martin Braun (1249080)
-- 4. Übung
-- Aufgabe 2
module Main where

class Concat a where
    (+++) :: a -> a -> a
    cat :: [a] -> a
    cat (x:[]) = x
    cat (x:xs) = x +++ (cat xs)
    
instance Concat Integer where
    (+++) x y = x + y

instance Concat [a] where
    (+++) x y = concat [x, y]
    
main = do
    print (cat [(toInteger 1), toInteger(2)])
    print (cat [[1], [2]])