
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module RTree (findIntersecting, findOverlapping) where

import Control.Applicative
import Data.Vector (Vector)
import qualified Data.Vector as V
import Control.Concurrent.STM

import Bounds
import TStore

{- Replace this with another type-natural library -}
import Natural

{- Rtree is transactional vector of tagged pointers -}

type VecBoundTagged b a = Vector (BoundTagged b a)

data RTree :: Nat -> * -> * where
  Leaf :: TVar (VecBoundTagged a (KeyT a)) -> RTree (S Z) a
  Node :: TVar (VecBoundTagged a (RTree n a)) -> RTree (S n) a

type R a = (TStored a, Spatial a)

{- Utility functions -}

matchIntersecting :: (BoundsT a -> Bool) -> VecBoundTagged a b -> Vector b
matchIntersecting predicate = V.map getElem . V.filter (predicate . getBounds)

foldDF :: 
  R a => 
  (BoundsT a -> Bool) -> (b -> KeyT a -> STM b) -> 
  b -> RTree n a -> STM b 
foldDF predicate accum initial node = 
  case node of
    Leaf vecVar -> do
      vec <- readTVar vecVar
      let matches = matchIntersecting predicate vec
      V.foldM accum initial matches
    Node vecVar -> do
      vec <- readTVar vecVar
      let matches = matchIntersecting predicate vec
      V.foldM (foldDF predicate accum) initial matches

foldDFPairs :: 
  R a => 
  (BoundsT a -> BoundsT a -> Bool) -> (b -> KeyT a -> KeyT a -> STM b) -> 
  b -> RTree n a -> RTree n a -> STM b
foldDFPairs predicate2 accum2 initial nodeA nodeB = 
  case (nodeA, nodeB) of
    (Leaf vecVarA, Leaf vecVarB) -> do
      vecA <- readTVar vecVarA
      vecB <- readTVar vecVarB
      undefined
    (Node vecVarA, Node vecVarB) -> do undefined

bestIndex :: R a => BoundsT a -> VecBoundTagged a b -> Int 
bestIndex target vec = 
  let increase element = size (cover target element) - size element
  in V.minIndex $ V.map (increase . getBounds) vec

locateCurrentPath :: R a => KeyT a -> RTree n a -> STM (Maybe (VecNat n Int))
locateCurrentPath target node = 
  case node of
    Leaf vecVar -> do
      vec <- readTVar vecVar 
      return $ flip Cons Nil <$> V.findIndex ((==target) . getElem) vec

locateBestPath :: R a => KeyT a -> RTree n a -> VecNat n Int
locateBestPath target node = 
  case node of
    Leaf vecVar -> undefined
    Node vecVar -> undefined

{- Read-only operations -}

findIntersecting :: R a => BoundsT a -> RTree n a -> STM [KeyT a]
findIntersecting query = foldDF (intersects query) (\ks k -> return (k:ks)) []

findOverlapping :: R a => BoundsT a -> RTree n a -> STM [(KeyT a, KeyT a)]
findOverlapping = undefined
