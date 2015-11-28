-- Martin Braun, 1249080
-- Aufgabe 2.3
module Main where

import Prelude

plus42 :: [Int] -> [Int]
plus42 [x] = [x + 42]
plus42 (x:xs) = (x + 42) : (plus42 xs)

main = do
    print (plus42 [1,2,3,4,5])
