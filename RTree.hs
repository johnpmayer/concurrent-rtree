
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

{-# LANGUAGE ScopedTypeVariables #-}

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

  let tryAccum :: R a => KeyT a -> Maybe (VecNat (S n) Int) -> (Int, RTree n a) -> STM (Maybe (VecNat (S n) Int))
      tryAccum (target' :: KeyT a) mLocated (index, childNode :: RTree n a) = 
        case mLocated of
          Just _ -> return mLocated
          Nothing -> do
            childLocated <- locateCurrentPath target' childNode
            return $ Cons index <$> childLocated

  in case node of
    Leaf vecVar -> do
      vec <- readTVar vecVar 
      return $ flip Cons Nil <$> V.findIndex ((==target) . getElem) vec
    Node (vecVar :: TVar (VecBoundTagged a (RTree n a))) -> do
      vec <- readTVar vecVar
      query <- (bounds :: a -> BoundsT a) <$> (unstore :: KeyT a -> STM a) target 
      let matches = V.indexed $ matchIntersecting (intersects query) vec
      V.foldM (tryAccum target) Nothing matches

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
