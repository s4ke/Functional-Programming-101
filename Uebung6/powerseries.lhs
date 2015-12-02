Living up McIlroy's music of the streams.

\begin{code}
{-# OPTIONS_GHC -XTypeSynonymInstances -XFlexibleInstances -XOverlappingInstances #-}
module PowerSeries where

import Test.QuickCheck
import Data.Ratio

-- dark magic
default (Integer, Rational, Double)
\end{code}

\begin{works}
scale x (y:ys) = x*y : (scale x ys)

conv (x:xs) (y:ys) = x*y : (x `scale` ys + xs `conv` (y:ys) )

instance Num a => Num [a] where
  (+) = zipWith (+)
  (*) = conv
\end{works}
\begin{code}
infixl 7 .*
x .* (y:ys) = x*y : x.*ys
_ .* [] = []

instance Num a => Num [a] where
  (+) = zipWith (+)
  (-) = zipWith (-)
  (x:xs) * g@(y:ys) = x*y : (x.*ys + xs*g )
  _ * _ = []
  fromInteger x = (fromIntegral x) : repeat 0

instance (Eq a, Fractional a) => Fractional [a] where
  (0:xs) / (0:ys) = xs/ys                       
  (x:xs) / (y:ys) = let q = x/y 
                    in q : (xs - q.*ys)/(y:ys)
  (/) _ _ = []             


helper op (g:gs) n = g`op`n : (helper op gs (n+1))
helper _ _ _ = []

integral fs = 0 : (helper (/) fs 1) 
deriv (f:fs) = helper (*) fs 1
deriv _ = []
\end{code}

Das war die Vorbereitung. Das muss man nicht verstehen.

Jetzt kommen die Definitionen, das sind Potenzreihenentwicklungen von entspr. Fu'n.

\begin{code}

expx = 1 + (integral expx)

instance (Eq a, Fractional a) => Floating [a] where 
    sqrt (0:0:fs) = 0 : sqrt fs 
    sqrt (1:fs) = qs where
        qs = 1 + integral((deriv (1:fs))/(2.*qs))

-- some numbers
ts = 1 : ts^2
\end{code}

Streams paper:
\begin{code}
revert (0:fs) = r
    where r = 0:1/(fs `comp` r)

comp [] _ = []
comp (x:xs) g@(0:ys) = (x:g) * (xs `comp` ys)
comp (x:xs) g@(y:ys) = ((fromIntegral x) + (y .* (xs`comp`g))) + ((0:ys) * (xs `comp` g))
-- meh

fastint [] = []
fastint f = 0 : (h 1 f) 
    where h _ [] = []
          h n (g:gs) = g/n : (h (n+1) gs)

testint n | n>0 && n<300 = (take n (fastint ts)) == (take n (integral ts))
          | otherwise = True

-- are these Bell numbers?
bell = 1 : (bell * expx)
\end{code}

\begin{code}
pow xs 0 = xs
pow xs n | n`mod`2 == 0 = t*t
         where t = pow xs (n`div`2)
pow xs n = xs * pow xs (n-1)

-- helper
fac n = product [1..n]
binom n k = (fac n) `div` ((fac k) * (fac (n-k)))

\end{code}
-- Touchard polynomials
toux :: Integer -> [Rational]
toux n = (serx n) / expx 

serx :: Integer -> [Rational]
serx n = [ k^n / (fac k) | k<-[0..] ]
 
tangent numbers: BROKEN???
\begin{code}
-- tnx = (sinx + (0:cosx)) / (cosx - (0:sinx))
\end{code}

Nicer output.

\begin{code}
instance Show Rational where
  show val = show (numerator val) ++ "/" ++ show (denominator val)
\end{code}
