-- Martin Braun
-- Aufgabe 3.4
module Main where

import Prelude
import Gen

maxPair :: [(Int, String)] -> String
maxPair x = snd ( foldl singMax (-1, "") x )
    where singMax (num1, str1) (num2, str2)
            | num1 > num2 = (num1, str1)
            | otherwise = (num2, str2)

main = do
    let seed = 42
    let values = kvGen seed 20
    print ( values )
    print ( maxPair values )