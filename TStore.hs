
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module TStore where

import Control.Concurrent.STM

class Eq (KeyT a) => TStored a where
  type KeyT a
  unstore :: KeyT a -> STM a

