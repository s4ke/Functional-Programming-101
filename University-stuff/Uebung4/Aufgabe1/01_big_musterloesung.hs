{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
-- so kann ich Compilerflags und Erweiterungen anfordern

module TheEvenBiggerInterface where

import qualified Prelude
import Prelude (Bool, Int, Ord, id, flip)

import qualified Data.List as L
import qualified Data.Set  as S

-- schönheitshalber
import Data.Set (Set)

-- die "komplizierte" Lösung
-- multiparameter Typklassen mit Functional Dependencies
-- weitere Informationen und Alternativen
-- https://wiki.haskell.org/Multi-parameter_type_class
-- https://wiki.haskell.org/Type_families
-- https://wiki.haskell.org/GADTs_for_dummies

-- c a -> c (c wird doch a eindeutig bestimmt)
class Col2 c a | c a -> c where
    insert :: Ord a => a -> c a -> c a
             -- Ord Kontext wegen Set nötig
    at :: Ord a => c a -> Int -> a
    filter :: (a -> Bool) -> c a -> c a
    singleton :: a -> c a
    null :: c a -> Bool
    size :: c a -> Int
    fromList :: Ord a => [a] -> c a
    map :: (Ord a, Ord b) => (a -> b) -> c a -> c b
    -- foldr hier:
    fold :: (a -> b -> b) -> b -> c a -> b

instance Col2 [] a where
    insert = L.insert
             -- Achtung, L.insert nimmt eine geordnete Liste an!
    at = (L.!!)
         -- so funktioniert qualified Aufruf bei Operatoren
    filter = L.filter
    singleton x = [x]
    null = L.null
    size = L.length
    fromList = id
    map = L.map
    fold = L.foldr

instance Col2 Set a where
    insert = S.insert
    at = flip S.elemAt
    filter = S.filter
    singleton = S.singleton
    null = S.null
    size = S.size
    fromList = S.fromList
    map = S.map
    fold = S.fold
