{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Main where

import Foreign.C.Types

#include <pcre.h>

foreign import ccall "math.h sin"
 c_sin :: CDouble -> CDouble
 
fastsin :: Double -> Double
fastsin = realToFrac . c_sin . realToFrac

main = do
    print $ fastsin (3.14 :: Double)