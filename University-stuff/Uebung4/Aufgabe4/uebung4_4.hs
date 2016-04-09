--Martin Braun
--Aufgabe 4
--Ich verstehe die Fehlermeldung nicht:
--   No instance for (GHC.Base.Applicative (Either String))
--      arising from the superclasses of an instance declaration
--    In the instance declaration for `Monad (Either String)'
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Control.Applicative
import Control.Monad
import Prelude (print, String)

-- muss hier selbst definiert werden da 
-- wenn man es importieren wuerde, gaebe
-- es Konflikte beim erstellen der Monade
data Either a b = Left a | Right b
                    deriving Show

instance Functor (Either String) where
    fmap f (Right x) = Right (f x)
    fmap f (Left x) = Left x

instance Applicative (Either String) where
    pure = return
    (<*>) (Right f) (Right x) = Right (f x)

instance Monad (Either String) where
    return b = Right b
    (Left s) >>= k = Left s
    (Right r) >>= k = k r


main = do
    print "Toast"