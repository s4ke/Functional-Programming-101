-- Martin Braun, 1249080
-- Aufgabe 3.1
module Main where

import Prelude

--Typ hier stimmt mit der Angabe überein
flip31 f x y z = f y x z
--Typ hier stimmt auch mit der Angabe überein
flip32 f x y z = f x z y
--Dieser Typ ist nicht das gleiche wie in der Angabe.
--Aber eigentlich kann doch nichts anderes geswitcht werden mehr.
--Angabe nicht korrekt?
flip33 f x y z = f z y x

toast :: Int -> Int -> Int -> Int
toast x y z = x - y - z

main = do
    print (flip31 toast 1 2 3)
    print (flip32 toast 1 2 3)
    print (flip33 toast 1 2 3)
