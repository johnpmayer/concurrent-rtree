
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Bounds where

class (Ord (UnitsT b), Num (UnitsT b)) => Bounds b where
  type UnitsT b
  intersects :: b -> b -> Bool
  contains :: b -> b -> Bool
  cover :: b -> b -> b
  size :: b -> UnitsT b

class Bounds (BoundsT a) => Spatial a where
  type BoundsT a
  bounds :: a -> BoundsT a

data BoundTagged b a = BoundTagged {
  getBounds :: BoundsT b,
  getElem :: a
}
