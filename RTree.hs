
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module RTree where

import Control.Applicative
import Bounds
import Data.Vector (Vector)
import qualified Data.Vector as V
import Control.Concurrent.STM

{- Replace this with another type-natural library -}

data N = S N | Z
data VecN :: N -> * -> * where
  Nil :: VecN Z a
  Cons :: a -> VecN n a -> VecN (S n) a

{- Rtree is transactional vector of tagged pointers -}

type VecBoundTagged b a = Vector (BoundTagged b a)

data Stored a = Stored {
  getStored :: STM a
}

data RTree :: N -> * -> * where
  Leaf :: TVar (VecBoundTagged a (Stored a)) -> RTree Z a
  Node :: TVar (VecBoundTagged a (RTree n a)) -> RTree (S n) a

foldDepthFirst :: 
  (Spatial a) => (BoundsT a -> Bool) -> (b -> a -> b) -> b -> 
  RTree n a -> STM b 
foldDepthFirst predicate accum initial node = 
  case node of
    Leaf vecVar -> do
      vec <- readTVar vecVar
      let accumM acc element = accum acc <$> getStored element
      let matches = V.map getElem . V.filter (predicate . getBounds) $ vec 
      V.foldM accumM initial matches
    Node vecVar -> do
      vec <- readTVar vecVar
      let matches = V.map getElem . V.filter (predicate . getBounds) $ vec 
      V.foldM (foldDepthFirst predicate accum) initial matches

findIntersecting :: (Spatial a) => BoundsT a -> RTree n a -> STM [a]
findIntersecting query = foldDepthFirst (intersect query) (flip (:)) []
