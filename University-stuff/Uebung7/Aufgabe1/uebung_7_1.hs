module Main where

import Prelude
import Criterion.Main

foldl' f z [] = z
foldl' f z (x:xs) = let z' = z `f` x
    in seq z' $ foldl' f z' xs

forAllRecExplicit :: (a -> Bool) -> [a] -> Bool
forAllRecExplicit p [] = True
forAllRecExplicit p (x:xs) | (p x) == True = forAllRecExplicit p xs
        | otherwise = False
        
forAllRecImplicit :: (a -> Bool) -> [a] -> Bool
forAllRecImplicit p [] = True
forAllRecImplicit p (x:xs) = (p x) && forAllRecImplicit p xs

forAllFoldl :: (a -> Bool) -> [a] -> Bool
forAllFoldl p xs = foldl (\x y -> x && p y) (True) xs

-- order is important here (p y) must be computed
-- before the evaluation of x
forAllFoldr :: (a -> Bool) -> [a] -> Bool
forAllFoldr p xs = foldr (\y x -> p y && x) (True) xs

forAllFoldl' :: (a -> Bool) -> [a] -> Bool
forAllFoldl' p xs = foldl' (\x y -> x && p y) (True) xs

toInt False = 0
toInt True = 1

-- Die Performance von Foldr sollte hier besser sein,
-- da nicht das komplette Array ausgewertet wird bevor gerechnet wird. Aber
-- irgendwie sind die Werte die Criterion zurücklieft totaler Mist.
main = defaultMain [
      bench "RecExplicit" $ whnf toInt ( forAllRecExplicit odd [1..1000000] ),
      bench "RecImplicit" $ whnf toInt ( forAllRecImplicit odd [1..1000000] ),
      bench "Foldl" $ whnf toInt ( forAllFoldl odd [1..1000000] ),
      bench "Foldr" $ whnf toInt ( forAllFoldr odd [1..1000000] ),
      bench "Foldl'" $ whnf toInt ( forAllFoldl' odd [1..1000000] )
    ]