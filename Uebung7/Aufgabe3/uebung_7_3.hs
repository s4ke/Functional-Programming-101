-- Martin Braun 1249080
-- Übung 7 Aufgabe 3
module Main where

import Prelude
import Control.DeepSeq

data Trie a = Node a (Trie a, Trie a, Trie a) | Elem a

hack () () () = ()

instance NFData (Trie a) where
    rnf (Elem x) = ()
    rnf (Node a (x, y, z)) = hack (rnf x) (rnf y) (rnf y)
    

main = do
    print "Toast"