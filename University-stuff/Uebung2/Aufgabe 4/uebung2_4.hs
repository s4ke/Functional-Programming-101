-- Martin Braun, 1249080
-- Aufgabe 2.4
module Main where

import Prelude

similarPair :: (Int, Int) -> (Int, Int) -> Bool
similarPair (a1,a2) (b1,b2) = (a1 == b1 || a1 == b2) && (a2 == b1 || a2 == b2)

main = do
    print (similarPair (3,4) (4,3))
    print (similarPair (3,4) (3,5))
