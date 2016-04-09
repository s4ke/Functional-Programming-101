-- Fooling around with Instances for a Binary Tree
-- inspired by learnyouahaskell.com
--
-- (C) Martin Braun 2016
module Main where

import Prelude
import qualified Data.Foldable as F

testTree :: Tree Integer
testTree = Node 5  
            (Node 3  Empty (
                        Node 9 Empty (Node 8 Empty Empty)
                    )
            )
            (Node 10 Empty Empty)

fnTree = Node (*3)  
            (Node (subtract 9)  Empty (
                        Node (+5) Empty (Node (+3) Empty Empty)
                    )
            )
            (Node (*0) Empty Empty) 

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

instance Monad Tree where
    return x = Node x Empty Empty
    Empty >>= f = Empty
    (Node x left right) >>= f = 
        let (Node y leftY rightY) = f x
        -- to get the correct in-order traversion
        -- all new left elements are used as smaller than the old left ones
        -- and all new right elements are used as greater than the old right ones
        in Node y (leftY `mappend` (left >>= f)) ((right >>= f) `mappend` rightY)
        
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
    printAsList testTree
    putStrLn $ show $ F.foldMap (pure :: a -> [a]) (fmap (+5) testTree)
    putStrLn $ show $ F.foldMap (pure :: a -> [a]) (fnTree <*> testTree)
    putStrLn $ show $ F.foldl (+) 0 $ F.foldMap (pure :: a -> [a]) (fnTree <*> testTree)
    putStrLn $ show $ F.foldl (+) 0 (fnTree <*> testTree)
    printAsList $ testTree `mappend` testTree
    printAsList $ (*) <$> testTree <*> testTree
    putStrLn ""
    
    putStrLn "return x >>= f Monad law"
    print $ return 2 >>= (\x -> Node x Empty Empty)
    print $ (\x -> Node x Empty Empty) 2
    putStrLn ""
    
    putStrLn "m >>= return = m Monad law"
    print $ testTree
    print $ testTree >>= return
    putStrLn ""
    
    putStrLn "(m >>= f) >>= g = m >>= (\\x -> f x >>= g)"
    print $ testTree  >>= (\x -> Node (x + 1) Empty Empty) >>= (\x -> Node (x * 2) Empty Empty)
    print $ testTree  >>= (\x -> (Node (x + 1) Empty Empty) >>= (\x -> Node (x * 2) Empty Empty))