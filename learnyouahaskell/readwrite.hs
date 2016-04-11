-- Simple program that reads the first line of the given file
-- prints it to the screen and then reads the first line of user
-- input and saves it into the file
--
-- (C) Martin Braun 2016
module Main where

import Prelude
import System.IO
import System.Environment
import Control.Monad
import Control.Exception

whenM :: (Monad f) => (f Bool) -> f () -> f()
whenM cond action = do  val <- cond
                        when val action

main = do
    args <- getArgs
    bracket (openFile (head args) ReadWriteMode)
            hClose
            (\h -> do
                    whenM ( liftM not $ hIsEOF h ) (hGetLine h >>= putStrLn)
                    hSeek h AbsoluteSeek 0
                    getLine >>= hPutStrLn h
            )