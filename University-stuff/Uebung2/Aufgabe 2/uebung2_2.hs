-- Martin Braun, 1249080
-- Aufgabe 2.2
module Main where

import Prelude hiding (filter)

filterListC :: (a -> Bool) -> [a] -> [a]
filterListC f xs = [x | x <- xs, f x]

filterMapConcat :: (a -> Bool) -> [a] -> [a]
filterMapConcat f xs = concat (map (\x -> if (f x) then [x] else []) xs)

filterFoldrConcat :: (a -> Bool) -> [a] -> [a]
filterFoldrConcat f xs = foldr (\x y -> if (f x) then concat [y, [x]] else y) [] xs

is42 :: Int -> Bool
is42 42 = True
is42 _ = False

main = do
    print (filterListC is42 [42, 123, 42])
    print (filterMapConcat is42 [42, 123, 42])
    print (filterFoldrConcat is42 [42, 123, 42])
