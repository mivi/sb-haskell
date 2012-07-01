{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

data Tree a b = Node (Tree a b) (Tree a b)
              | LLeaf a
              | RLeaf b deriving (Show)
                                 
instance (Eq a, Eq b) => Eq (Tree a b) where
  (Node x y) == (Node w z) = (x==w) && (y ==z)
  LLeaf x == LLeaf y = (x==y)
  RLeaf x == RLeaf y = x==y
  _ == _ = False
  
instance Functor (Tree a b) where
  fmap f (Node l r) = Node (fmap f l) (fmap f r)
  fmap f (LLeaf x) = LLeaf (f x)
  fmap f (RLeaf x) = RLeaf (f x)

class Wrap a where
  getlength :: a -> Int

-- instance (Integral a) => Wrap a where
--   getlength a = (fromIntegral a)::Int
instance Wrap Int where
  getlength a = a
  
instance Wrap [Char] where
  getlength=length


-- getlength :: String -> Int
-- getlength =length
-- getlength :: Int -> Int
-- getlength a = a