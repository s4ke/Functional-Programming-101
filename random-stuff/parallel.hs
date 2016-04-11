module Main where

import Control.Parallel

fibSeq :: Integer -> Integer
fibSeq 0 = 1
fibSeq 1 = 1
fibSeq x = (fibSeq (x - 1)) + (fibSeq (x - 2))

fib :: Integer -> Integer
fib n | n < 10 = fibSeq n
    | otherwise
        = r `par` (l `pseq` l + r)
        where l = fib (n-1)
              r = fib (n-2)

main = do
    print $ fib 33
