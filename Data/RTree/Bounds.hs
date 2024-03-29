
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.RTree.Bounds where

class (Ord (UnitsT b), Num (UnitsT b)) => Bounds b where
  type UnitsT b
  empty :: b
  intersects :: b -> b -> Bool
  contains :: b -> b -> Bool
  cover :: b -> b -> b
  size :: b -> UnitsT b

data Bounded b a = Bounded 
  { getBounds :: BoundsT b
  , getElem :: a
  } 

class Bounds (BoundsT a) => Spatial a where
  type BoundsT a
  bounds :: a -> BoundsT a

