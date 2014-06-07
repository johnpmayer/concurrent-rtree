
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeFamilies #-}

module Box2f where

import Bounds

data Box2f = Bounds2F {
  minX :: Float,
  maxX :: Float,
  minY :: Float,
  maxY :: Float
}

instance Bounds Box2f where
  data UnitsT Box2f = Float
  intersect b1 b2 = undefined
  contains b1 b2 = undefined
  cover b1 b2 = undefined
  size b = undefined
