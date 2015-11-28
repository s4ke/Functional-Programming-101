-- Martin Braun (1249080)
-- 4. Übung
-- Aufgabe 1
--
-- Alle Funktionen, für die man hier auf spezielle Elemente
-- der jeweiligen Datenstruktur zugreifen möchte, gibt es hier
-- jeweils nur eine Char Methode (die Datenstrukturen sind auf Char
-- beschränkt hier), da sonst funktionale Abhängigkeiten benötigt
-- werden würden.
--
-- Bonusaufgabe:
-- Kann nicht gemacht werden, da sonst funktionale Abhängigkeiten
-- gebraucht werden würden.
module Main where

import Prelude hiding (odd, filter)
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Char as C

class Combined a where
    insert :: Char -> a -> a
    at :: Int -> a -> Char
    filter :: (Char -> Bool) -> a -> a
    singleton :: a -> Bool
    singleton a = (size a) == 1
    null :: a -> Bool
    null a = (size a) == 0
    size :: a -> Int
    fromList :: [Char] -> a
    map :: (Char -> Char) -> a -> a
    fold :: (Char -> Char -> Char) -> Char -> a -> Char

instance Combined [Char] where
    insert x a = L.insert x a
    at x a = a!!x
    filter f a = [x | x <- a, f x]
    size a = L.length a
    fromList x = x
    map f a = L.map f a
    fold f x a = L.foldr f x a
    
instance Combined (S.Set Char) where
    insert x a = S.insert x a
    at x a = at x (S.toList a)
    filter f a = fromList (filter f (S.toList a))
    size a = S.size a
    fromList x = S.fromList x
    map f a = S.map f a
    fold f x a = S.fold f x a
    
odd :: Char -> Bool
odd c = (x `mod` 2) == 1 where x = C.digitToInt c

some :: Char -> Char -> Char
some x y = y
    
main = do 
    let x = ['1', '2', '3', '4']
    print x
    let y = insert '5' x
    print y
    print (at 1 y)
    print (filter odd y)
    print (Main.null y)
    print (Main.singleton y)
    print (Main.size y)
    print (fold some 'A' y)
    let ( x2 :: S.Set Char ) = Main.fromList x
    print  x2
    let y2 = insert '5' x2
    print y2
    print (at 1 y2)
    print (filter odd y2)
    print (Main.null y2)
    print (Main.singleton y2)
    print (Main.size y2)
    print (fold some 'A' y2)