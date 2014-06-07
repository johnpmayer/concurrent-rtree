
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-} -- Spatial/Bounds constraint

module Bounds where

import Control.Concurrent.STM

class Bounds b where
  data UnitsT b
  intersect :: b -> b -> Bool
  contains :: b -> b -> Bool
  cover :: b -> b -> b
  size :: b -> UnitsT b

class Bounds (BoundsT a) => Spatial a where
  data BoundsT
  bounds :: a -> BoundsT a

data BoundTagged b a = BoundTagged {
  getBounds :: BoundsT b,
  getElem :: a
}
