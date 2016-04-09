-- Martin Braun (1249080)
-- 4. Übung
-- Aufgabe 3
module Main where

import Prelude

list2ones :: [a] -> [Int]
list2ones x = map toOne x where 
                toOne _ = 1

average :: Fractional a => [a] -> a
average xs = go xs 0 0 where
                go [] sum length = sum / length
                go (x:xs) sum length = go xs (x + sum) (length + 1)

main = do
    print (list2ones [1,2,3,4,5])
    print (average [1,2,3,4,5])