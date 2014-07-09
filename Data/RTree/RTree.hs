
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RankNTypes #-}

module Data.RTree.RTree where

import Data.RTree.Internal.RTree as I

data RTree a = RTreeRoot
  { rConfig :: I.RConfig
  , rootNOde :: forall h. I.RTree h a
  }

