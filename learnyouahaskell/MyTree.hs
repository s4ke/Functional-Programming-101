-- Fooling around with Instances for a Binary Tree
-- inspired by learnyouahaskell.com
--
-- (C) Martin Braun 2016
module Main where

import Prelude
import qualified Data.Foldable as F 
import Control.Applicative 

testTree :: Tree Integer
testTree = Node 5  
            (Node 3  
                (Node 1 Empty Empty)  
                (Node 6 Empty Empty)  
            )  
            (Node 9  
                (Node 8 Empty Empty)  
                (Node 10 Empty Empty)  
            )

fnTree = Node (+5)
            (Node (*3)  
                (Node (subtract 1) Empty Empty)  
                (Node (+6) Empty Empty)  
            )  
            (Node (+9)  
                (Node (subtract 8) Empty Empty)  
                (Node (*10) Empty Empty)  
            )       

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

instance F.Foldable Tree where
    foldMap f Empty = mempty
    foldMap f (Node elem left right) = 
        foldMap f left `mappend`
        f elem `mappend`
        foldMap f right
        
instance Functor Tree where
    fmap f Empty = Empty
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)
    
instance Applicative Tree where
    pure x = Node x Empty Empty
    Empty <*> _ = Empty
    _ <*> Empty = Empty
    Node f left1 right1 <*> Node x left2 right2 =
        Node (f x) (left1 <*> left2) (right1 <*> right2)
        
instance Monoid (Tree a) where
    mempty = Empty
    x `mappend` Empty = x
    Empty `mappend` x = x
    parent `mappend` child = insert parent child
        where
            insert (Node x left Empty) child = Node x left child
            insert (Node x left right) child = Node x left $ insert right child
            
printAsList tree = putStrLn $ show $ F.foldMap (pure :: a -> [a]) tree
        
main = do
    putStrLn $ show $ F.foldMap (pure :: a -> [a]) (fmap (+5) testTree)
    putStrLn $ show $ F.foldMap (pure :: a -> [a]) (fnTree <*> testTree)
    putStrLn $ show $ F.foldl (+) 0 $ F.foldMap (pure :: a -> [a]) (fnTree <*> testTree)
    putStrLn $ show $ F.foldl (+) 0 (fnTree <*> testTree)
    printAsList $ testTree `mappend` testTree