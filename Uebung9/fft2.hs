-- Martin Braun 1249080
module Main where

import Prelude
import Data.List

fftSkel (n , xs , w ) = dc isTrivial divide trivial combine (n , xs , w )
    where 
        divide (n , xs , w ) = [( k , ls , ww ) , (k , mods , ww )]
            where (( _ , ls ) , (k , rs )) = splitHalves (n , xs )
                  (_ , mods ) = (k , rs ) @* ( powers k ww )
                  ww = w*w
        isTrivial (n , _ , _ ) = n == 1
        trivial (n , xs , _ ) = (n , xs )
        combine = concat . transpose
        
dc isTrivial divide trivial combine cur
    | (isTrivial cur) = trivial cur
    | otherwise = combine (dc isTrivial divide trivial combine left)
                          (dc isTrivial divide trivial combine right)
        where [left , right] = divide cur
        
{-  
    hier fehlt mir leider der Mathematische Hintergrund :D
    Alles was ich hier machen würde, wäre nur geraten.
    Hätte anscheinend doch in die Übung gehen sollen.
    splitHalves = 
    
    powers = 
-}
        
x @+ y = zipWith (+) x y
x @- y = zipWith (-) x y
x @* y = zipWith (*) x y

main = do
    print "Leider nicht testbar, da Funktion nicht fertig implementiert. Dafür aber dc"