module Main where

import Prelude
import System.Environment

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib x = (fib (x - 1)) + (fib (x - 2))

main = do
    getArgs >>= (return . head) >>= (return . read) >>= (return . fib) >>= print
    getArgs >>= return . fib . read . head >>= print
    return (fib . read . head) <*> getArgs >>= print
    (fib . read . head) <$> getArgs >>= print