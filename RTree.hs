
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RTree where

import Control.Applicative
import Control.Monad
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

matchPred :: (BoundsT b -> Bool) -> VecBoundTagged b a -> Vector a
matchPred predicate = V.map getElem . V.filter (predicate . getBounds)

foldDF :: 
  R a => 
  (BoundsT a -> Bool) -> (b -> KeyT a -> STM b) -> 
  b -> RTree n a -> STM b 
foldDF predicate accum initial node = 
  case node of
    Leaf vecVar -> do
      vec <- readTVar vecVar
      let matches = matchPred predicate vec
      V.foldM accum initial matches
    Node vecVar -> do
      vec <- readTVar vecVar
      let matches = matchPred predicate vec
      V.foldM (foldDF predicate accum) initial matches

matchPred2 :: 
  (BoundsT b -> BoundsT b -> Bool) -> 
  VecBoundTagged b a -> VecBoundTagged b a -> 
  [(a,a)]
matchPred2 predicate2 vec1 vec2 = 
  V.foldl (\acc1 e1 -> 
    V.foldl (\acc2 e2 -> if predicate2 (getBounds e1) (getBounds e2) 
                         then (getElem e1, getElem e2) : acc2 
                         else acc2) acc1 vec2) [] vec1



foldDFPairs :: 
  R a => 
  (BoundsT a -> BoundsT a -> Bool) -> (b -> (KeyT a, KeyT a) -> STM b) -> 
  b -> RTree n a -> RTree n a -> STM b
foldDFPairs predicate2 accum2 initial nodeA nodeB = 
  case (nodeA, nodeB) of
    (Leaf vecVarA, Leaf vecVarB) -> do 
      vecA <- readTVar vecVarA
      vecB <- readTVar vecVarB
      let matches = matchPred2 predicate2 vecA vecB
      foldM accum2 initial matches
    (Node vecVarA, Node vecVarB) -> do 
      vecA <- readTVar vecVarA
      vecB <- readTVar vecVarB
      let matches = matchPred2 predicate2 vecA vecB
          doPair initial' (a,b) = foldDFPairs predicate2 accum2 initial' a b
      foldM doPair initial matches

bestIndex :: R a => BoundsT a -> VecBoundTagged a b -> Int 
bestIndex target vec = 
  let increase element = size (cover target element) - size element
  in V.minIndex $ V.map (increase . getBounds) vec

bestSplit :: VecBoundTagged b a -> (VecBoundTagged b a, VecBoundTagged b a)
bestSplit = undefined

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
      let matches = V.indexed $ matchPred (intersects query) vec
      V.foldM (tryAccum target) Nothing matches

{-
locateBestPath :: R a => KeyT a -> RTree n a -> VecNat n Int
locateBestPath target node = 
  case node of
    Leaf vecVar -> undefined
    Node vecVar -> undefined
    -}

{- Read-only operations -}

find :: R a => BoundsT a -> RTree n a -> STM [KeyT a]
find query = foldDF (intersects query) (\ks k -> return (k:ks)) []

findCollisions :: R a => RTree n a -> STM [(KeyT a, KeyT a)]
findCollisions node = foldDFPairs intersects 
                                  (\ks (ka,kb) -> return ((ka,kb):ks)) [] 
                                  node node

{- Read-write operations -}

insert :: R a => KeyT a -> RTree n a -> STM ()
insert target node = undefined target node

