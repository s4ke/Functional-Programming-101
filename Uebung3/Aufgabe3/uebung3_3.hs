-- Martin Braun
-- Aufgabe 3.3
-- Aus irgendeinem Grund bekomme ich hier keine negativen Werte
-- v.a. für stirling1 10 5 was eigentlich negativ sein sollte.
-- Ich bekomme sogar den richtigen Betrag heraus
module Main where

import Prelude

sterling1 :: Int -> Int -> Int
sterling1 x k 
    | x == k = 1
    | ( k == 0 && x > 0 ) || ( x < k ) = 0 
    | otherwise = ( sterling1 n ( k - 1 ) ) + ( n * ( sterling1 n k ) )
    where n = x - 1

sterling2 :: Int -> Int -> Int
sterling2 x k
    | x == k = 1
    | ( k == 0 && x > 0 ) || ( x < k ) = 0
    | otherwise = ( sterling2 n ( k - 1 ) ) + ( k * ( sterling2 n k ) )
    where n = x - 1

sterling :: Int -> Int -> Int -> Int
sterling n k t
    | t == 1 = sterling1 n k
    | t == 2 = sterling2 n k
    | otherwise = error "third argument (type) must be either 1 or 2"

main = do
    print ( sterling1 10 5 )
    print ( sterling2 10 5 )
    print ( sterling 10 5 1 )
    print ( sterling 10 5 2 )