-- Martin Braun, 1249080
-- 6. Uebung
-- Aufgabe 2
-- lazyness, infinite lists, power series
module Main where

import PowerSeries

cosx :: [Rational]
cosx = 1 - (integral sinx)

sinx :: [Rational]
sinx = integral cosx

main = do
    print $ take 10 (sinx)
    print $ take 10 (cosx)