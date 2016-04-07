-- Reverse Polish Notation calculator
-- inspired by learnyouahaskell.com
-- but with some quirks and error handling added
--
-- (C) Martin Braun 2016
--
-- unary functions can be added easily by creating
-- another map that contains all of them and then add
-- the handling code so that it is handled before or
-- after the currently binary functions
--
-- Numbers should have the lowest precedence.
module Main where

import Prelude
import System.Environment

functions :: [(String, (Double -> Double -> Double))]
functions = [("+", (\x y -> x + y)),
             ("-", (\x y -> x - y)),
             ("*", (\x y -> x * y)),
             ("/", (\x y -> x / y)),
             ("^", (\x y -> x ** y))]

solve :: String -> Either String Double
solve = head . check . foldl calc [] . words
    where   calc [Left x] next = [Left x]
            calc values next =
                case (lookup next functions) of
                    -- the symbol was a function symbol
                    (Just fn) -> let ((Right x):(Right y):rest) = values
                                 in (Right $ fn y x):rest
                    -- the symbol was either a number or it was malformed input
                    Nothing -> case (reads next :: [(Double, String)]) of
                                   (x, _):ys -> (Right x):values
                                   _ -> [Left $ next ++ " is no valid function symbol or number"]
            
            check [x] = [x]
            check ys = [Left $ "Error, the stack has to contain exactly one element at the end of the computation, was: " 
                                ++ show (map (\x -> either show show x) ys)]
    
main = do
    args <- getArgs
    putStrLn $ either show show $ solve $ head args