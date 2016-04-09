-- Martin Braun, 1249080
-- Aufgabe 2.1
module Main where

import Prelude hiding (head, tail, init, last)
import Data.Maybe

head :: [a] -> Maybe a
head [] = Nothing
head (x:xs) = Just x

tail :: [a] -> Maybe [a]
tail [] = Nothing
tail [x] = Just []
tail (x:xs) = Just xs

init :: [a] -> Maybe [a]
init [] = Nothing
init xs = Just (go xs)
    where
        go [x] = []
        go (x:xs) = x : (go xs)

last :: [a] -> Maybe a
last [] = Nothing
last [x] = Just x
last (x:xs) = last xs

main = do
    print [1,2,3,4]
    print (head [1,2,3,4])
    print (tail [1,2,3,4])
    print (init [1,2,3,4])
    print (last [1,2,3,4])
    
    print [1]
    print (head [1])
    print (tail [1])
    print (init [1])
    print (last [1])
    
    print ([] :: [Int])
    print (head ([] :: [Int]))
    print (tail ([] :: [Int]))
    print (init ([] :: [Int]))
    print (last ([] :: [Int]))

