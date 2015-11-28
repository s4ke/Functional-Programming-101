-- Martin Braun
-- Aufgabe 3.2
module Main where

import Prelude

powerNaive :: Integer -> Int -> Integer
powerNaive x 0 = 1
powerNaive x y = x * ( powerNaive x ( y - 1 ) )

powerSmart :: Integer -> Int -> Integer
powerSmart x y 
    | y == 0 = 1
    | odd y = x * ( powerSmart x ( y - 1 ) )
	| otherwise = ( powerSmart x ( div y 2 ) ) 
                * ( powerSmart x ( div y 2 ) )

main = do
    print (powerNaive 2 10)
    print (powerSmart 2 10)
    print (powerSmart 2 11)