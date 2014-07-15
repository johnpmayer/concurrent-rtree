
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}

module Data.RTree.RTree where

import Control.Concurrent.STM
import qualified Data.Vector as V

import qualified Data.RTree.Internal.RTree as I
import Data.RTree.Internal.RTree (RInsert(..))
import Data.RTree.TStore

data RRoot a = forall h. Root (I.RTree h a)

data RTree a = Tree
  { getConfig :: I.RConfig
  , getRootVar :: TVar (RRoot a)
  }

defaultEmpty :: STM (RTree a)
defaultEmpty = do
  n <- I.empty
  rootVar <- newTVar $ Root n
  return $ Tree I.defaultConfig rootVar

upsert :: I.R a => KeyT a -> (a -> a) -> RTree a -> STM ()
upsert key f (Tree cfg rootVar) = do
  (Root node) <- readTVar rootVar
  result <- I.upsert cfg key f node
  case result of
    NoExpand -> return ()
    Expand _ -> return ()
    Split bt1 bt2 -> do
      let nodes = V.fromList [bt1, bt2]
      tNodes <- newTVar nodes
      writeTVar rootVar $ Root $ I.Node tNodes
