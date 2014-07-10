
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}

module Data.RTree.RTree where

import Control.Applicative
import Control.Concurrent.STM

import qualified Data.RTree.Internal.RTree as I
import Data.RTree.Internal.RTree (RInsert(..))
import Data.RTree.TStore

data RTree a = forall h. RTree
  { config :: I.RConfig
  , root :: I.RTree h a
  }

defaultEmpty :: STM (RTree a)
defaultEmpty = RTree I.defaultConfig <$> I.empty

upsert :: I.R a => KeyT a -> (a -> a) -> RTree a -> STM ()
upsert key f (RTree cfg node) = do
  result <- I.upsert cfg key f node
  case result of
    NoExpand -> return ()
    Expand _ -> return ()
    Split _ _ -> error . show $ result
