-- Martin Braun 1249080
module Main where

import Prelude
import Data.Complex

mix :: [a] -> [a] -> [a]
mix f g = concat $ zipWith (\x y -> [x, y]) f g

fft_do :: RealFloat a => [Complex a] -> Complex a -> [Complex a]
fft_do [k] w = [k]
fft_do f w = mix (fft_do (l @+ r) (w*w)) (fft_do ((l @- r)@* aw) (w*w))
  where (l, r) = split f
        n = length f
        aw = take n $ iterate (*w) 1

root_of_unity :: (RealFloat a, Integral b) => b -> b -> Complex a
root_of_unity j n = (exp (((2 * pi) / (toComplex n)) * (0 :+ 1)))

toComplex x = ( fromRational . toRational ) x :+ fromInteger 0

split xs = let m = (length xs) `div` 2
           in splitAt m xs

fftMy :: RealFloat a => [Complex a] -> [Complex a]
fftMy xs = fft_do xs (root_of_unity 1 (length xs))

x @+ y = zipWith (+) x y
x @- y = zipWith (-) x y
x @* y = zipWith (*) x y

main = do
    print "Toast"