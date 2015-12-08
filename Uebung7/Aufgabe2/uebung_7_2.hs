-- Martin Braun 1249080
-- Übung 7 Aufgabe 2
-- Keine Ahnung ob das die richtigen Redexes reprasesentiert
-- aber dafuer hab ich bisschen mit IO() Monaden rumgespielt :D
module Main where

import Prelude

printAll :: [IO()] -> IO()
printAll [] = putStrLn("done")
printAll (x:xs) = do
        x
        printAll xs

main = do
    printAll $ zipWith (\a b -> 
                            putStrLn ("Ausgewertet fuer die erste Liste: " ++ (show a) ++ 
                                    ", Ausgewertet fuer die zweite Liste: " ++ (show b) 
                                    ++ "\n" ++ "Ergebnis: " ++ (show $ (a + b))))
                       [1, 2, 3, 4, 5]
                       [1..]