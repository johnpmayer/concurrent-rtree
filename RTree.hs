
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RTree where

import Control.Applicative
import Control.Monad
import qualified Data.List as L
import Data.Vector (Vector)
import qualified Data.Vector as V
import Control.Concurrent.STM
import Prelude hiding (Bounded)

import Bounds
import TStore

import Natural -- Replace this with another type-natural library

{- A vector of spatial elements, tagged with bounds -}

type VecBound b a = Vector (Bounded b a)

vecCover :: Bounds (BoundsT b) => VecBound b a -> BoundsT b
vecCover = V.foldl cover Bounds.empty . V.map getBounds

{- Rtree is transactional vector of tagged pointers -}

data RConfig = RConfig 
  { maxElems :: Int
  , minElems :: Int
  }

data RTree :: Nat -> * -> * where
  Leaf :: TVar (VecBound a (KeyT a)) -> RTree Z a
  Node :: TVar (VecBound a (RTree n a)) -> RTree (S n) a

type R a = (TStored a, Spatial a)

matchPred :: (BoundsT b -> Bool) -> VecBound b a -> Vector a
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
  VecBound b a -> VecBound b a -> 
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

find :: R a => BoundsT a -> RTree n a -> STM [KeyT a]
find query = foldDF (intersects query) (\ks k -> return (k:ks)) []

findCollisions :: R a => RTree n a -> STM [(KeyT a, KeyT a)]
findCollisions node = foldDFPairs intersects 
                                  (\ks (ka,kb) -> return ((ka,kb):ks)) [] 
                                  node node

locateCurrentPath :: R a => Bounded a (KeyT a) -> RTree n a -> STM (Maybe (VecNat (S n) Int))
locateCurrentPath boundedKey node = 

  let tryAccum :: R a => Bounded a (KeyT a) -> Maybe (VecNat (S (S n)) Int) -> (Int, RTree n a) -> STM (Maybe (VecNat (S (S n)) Int))
      tryAccum target mLocated (index, childNode :: RTree n a) = 
        case mLocated of
          Just _ -> return mLocated
          Nothing -> do
            childLocated <- locateCurrentPath target childNode
            return $ Cons index <$> childLocated

  in case node of
    Leaf vecVar -> do
      vec <- readTVar vecVar 
      return $ singleton <$> V.findIndex ((== getElem boundedKey) . getElem) vec
    Node (vecVar :: TVar (VecBound a (RTree n a))) -> do
      vec <- readTVar vecVar
      let query = getBounds boundedKey
          matches = V.indexed $ matchPred (intersects query) vec
      V.foldM (tryAccum boundedKey) Nothing matches

bestIndex :: R a => BoundsT a -> VecBound a b -> Int 
bestIndex target vec = 
  let increase element = size (cover target element) - size element
  in V.minIndex $ V.map (increase . getBounds) vec

locateBestPath :: R a => BoundsT a -> RTree n a -> STM (VecNat (S n) Int)
locateBestPath target node = 
  case node of
    Leaf vecVar -> do
      vec <- readTVar vecVar      
      return $ singleton (bestIndex target vec)
    Node vecVar -> do
      vec <- readTVar vecVar
      let best = bestIndex target vec
          child = getElem $ vec V.! best
      rest <- locateBestPath target child
      return $ Cons best rest

-- Quadratic Split with minimum element checks
bestSplit :: Bounds (BoundsT b) => RConfig -> VecBound b a -> (VecBound b a, VecBound b a)
bestSplit cfg vec = 
  let pairs = [ (a,b) | a <- [0..(V.length vec - 1)], b <- [0..(a-1)] ]
      vBounds idx = getBounds $ vec V.! idx
      pairSize a b = size $ cover (vBounds a) (vBounds b)
      comparePairs a b = compare (uncurry pairSize a) (uncurry pairSize b)
      (worst1,worst2) = L.maximumBy comparePairs pairs
      remIndices = [a | a <- [0..(V.length vec - 1)], a /= worst1, a /= worst2]
      remaining = reverse $ [0 .. length remIndices - 1]
      segregate ((c1, c2),(l1,l2)) (tgt,remain) = 
        let size1 = size $ cover (vBounds worst1) (vBounds tgt)
            size2 = size $ cover (vBounds worst2) (vBounds tgt)
            forceLeft = remain + c1 == minElems cfg
            forceRight = remain + c2 == minElems cfg
        in
          if (size1 < size2 || forceLeft) && not forceRight
          then ((c1 + 1, c2),(tgt:l1,l2))
          else ((c1, c2 + 1),(l1,tgt:l2))
      (list1, list2) = snd $ foldl segregate ((1,1),([worst1],[worst2])) $ 
                        zip remIndices remaining
  in (V.map (vec V.!) $ V.fromList list1, V.map (vec V.!) $ V.fromList list2)

data RVecInsert b a 
  = VecInsert (VecBound b a) 
  | VecSplit (VecBound b a) (VecBound b a)

checkInsertVec :: Bounds (BoundsT b) => RConfig -> VecBound b a -> RVecInsert b a
checkInsertVec cfg smoosh =
  if V.length smoosh > maxElems cfg
  then uncurry VecSplit $ bestSplit cfg smoosh
  else VecInsert smoosh

data RInsert n a 
  = NoExpand
  | Expand (BoundsT a)  
  | Split (Bounded a (RTree n a)) (Bounded a (RTree n a))

insert :: R a => RConfig -> Bounded a (KeyT a) -> RTree n a -> STM (RInsert n a)
insert cfg boundedTarget node =
  case node of

    Leaf vecVar -> do
      vec <- readTVar vecVar
      let smoosh = V.cons boundedTarget vec
      case checkInsertVec cfg smoosh of
        VecInsert newVec -> do
          writeTVar vecVar newVec
          return $
            if vecCover vec `contains` getBounds boundedTarget
            then NoExpand
            else Expand $ vecCover newVec
        VecSplit newVec1 newVec2 -> 
          Split <$> (Bounded (vecCover newVec1) . Leaf <$> newTVar newVec1)
                <*> (Bounded (vecCover newVec2) . Leaf <$> newTVar newVec2)

    Node vecVar -> do
      vec <- readTVar vecVar
      let best = bestIndex (getBounds boundedTarget) vec
          boundedChild = vec V.! best
          child = getElem boundedChild
      childInsert <- insert cfg boundedTarget child
      case childInsert of
        NoExpand -> return NoExpand
        Expand newChildBounds -> do
          let newBoundedChild = Bounded newChildBounds child
              newVec = vec V.// [(best,newBoundedChild)]
          writeTVar vecVar newVec
          return $
            if vecCover vec `contains` newChildBounds
            then NoExpand
            else Expand $ vecCover newVec
        Split newChild1 newChild2 ->
          let leftRem = V.take best vec
              rightRem = V.drop (best + 1) vec
              smoosh = V.concat [ leftRem
                                , rightRem
                                , V.singleton newChild1
                                , V.singleton newChild2 ]
          in case checkInsertVec cfg smoosh of
            VecInsert newVec -> do 
              writeTVar vecVar newVec
              return $
                if vecCover vec `contains` getBounds boundedChild
                then NoExpand
                else Expand $ vecCover newVec
            VecSplit newVec1 newVec2 -> 
              Split <$> (Bounded (vecCover newVec1) . Node <$> newTVar newVec1)
                    <*> (Bounded (vecCover newVec2) . Node <$> newTVar newVec2)

update :: R a => KeyT a -> (a -> a) -> RTree n a -> STM ()
update key f (node :: RTree n a) = do
  (value :: a) <- derefStore key
  let (b :: BoundsT a) = bounds value
      boundedKey = Bounded b key
  (mPath :: Maybe (VecNat (S n) Int)) <- locateCurrentPath boundedKey node
  case mPath of
    Nothing -> undefined
    Just path -> 
      let newValue = f value
          (newB :: BoundsT a) = bounds newValue
          (newBoundedValue :: Bounded a a) = Bounded newB newValue
      in undefined 
 
markDead :: VecNat (S n) Int -> RTree n a -> STM ()
markDead = undefined

upsert :: R a => RConfig ->
  Bounded a (KeyT a) -> VecNat (S n) Int -> RTree n a -> 
  STM (RInsert n a)
upsert cfg newBoundedKey (Cons index path) node =
  case node of

    Leaf vecVar -> do
      vec <- readTVar vecVar 
      let newVec = vec V.// [(index, newBoundedKey)]
      writeTVar vecVar newVec
      return $
        if vecCover vec `contains` getBounds newBoundedKey
        then NoExpand
        else Expand $ vecCover newVec

    Node vecVar -> do
      vec <- readTVar vecVar
      let best = bestIndex (getBounds newBoundedKey) vec
          child = getElem $ vec V.! index
      childResult <- if best == index
      then upsert cfg newBoundedKey path child
      else do
        markDead path child
        insert cfg newBoundedKey child
      undefined childResult
