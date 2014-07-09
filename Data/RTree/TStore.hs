
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.RTree.TStore where

import Control.Concurrent.STM

class Eq (KeyT a) => TStored a where
  type KeyT a
  derefStore :: KeyT a -> STM a
  modStore :: KeyT a -> a -> STM ()

