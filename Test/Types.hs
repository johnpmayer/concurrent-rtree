
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Test.Types where

import Control.Concurrent.STM
import Prelude hiding (min,max)
import qualified Prelude as Pr

import Data.RTree.Bounds 
import Data.RTree.TStore

data Vec2F = Vec2F
  { x :: Float
  , y :: Float
  } deriving Show

data Box2F = Box2F
  { min :: Vec2F
  , max :: Vec2F
  }

instance Bounds (Maybe Box2F) where
  type UnitsT (Maybe Box2F) = Float
  empty = Nothing
  (Just box1) `intersects` (Just box2) = not $
    ( (x.max$box1) < (x.min$box2) ||
      (y.max$box1) < (y.min$box2) ||
      (x.max$box2) < (x.min$box1) ||
      (y.max$box2) < (y.min$box1) )
  _ `intersects` _ = False
  Nothing `contains` _ = False
  _ `contains` Nothing = True
  (Just box1) `contains` (Just box2) = 
    ( (x.min$box1) < (x.min$box2) &&
      (y.min$box1) < (y.min$box2) &&
      (x.max$box1) > (x.max$box2) &&
      (y.max$box1) > (y.max$box2) )
  cover Nothing b = b
  cover b Nothing = b
  cover (Just box1) (Just box2) = 
    let minx = Pr.min (x.min$box1) (x.min$box2)
        miny = Pr.min (y.min$box1) (y.min$box2)
        maxx = Pr.max (x.max$box1) (x.max$box2)
        maxy = Pr.max (y.max$box1) (y.max$box2)
    in Just $ Box2F (Vec2F minx miny) (Vec2F maxx maxy)
  size Nothing = 0
  size (Just box) = 
    let lx = (x.max$box) - (x.min$box)
        ly = (y.max$box) - (y.min$box)
    in lx * ly

data Ship = Ship
  { pos :: Vec2F
  , vel :: Vec2F
  , rad :: Float
  } deriving Show

instance Spatial Ship where
  type BoundsT Ship = Maybe Box2F
  bounds ship = 
    let minx = (x.pos$ship) - rad ship
        miny = (y.pos$ship) - rad ship
        maxx = (x.pos$ship) + rad ship
        maxy = (y.pos$ship) + rad ship
    in Just $ Box2F (Vec2F minx miny) (Vec2F maxx maxy)

data ActiveShip = ActiveShip
  { shipID :: Int
  , shipVar :: TVar Ship
  }

instance Eq ActiveShip where
  as1 == as2 = (shipID as1 == shipID as2)

instance TStored Ship where
  type KeyT Ship = ActiveShip
  derefStore active = readTVar (shipVar active)
  modStore active new = writeTVar (shipVar active) new

