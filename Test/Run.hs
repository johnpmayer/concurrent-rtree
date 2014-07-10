
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Test.Run where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.Random

import Data.RTree.RTree
import Test.Types

posRange, velRange, radRange :: (Float,Float)
posRange = (-100, 100)
velRange = (-5, 5)
radRange = (0, 10)

makeRandomShip :: MonadRandom m => m Ship
makeRandomShip = do
  px <- getRandomR posRange
  py <- getRandomR posRange
  vx <- getRandomR velRange
  vy <- getRandomR velRange
  r <- getRandomR radRange
  return $ Ship (Vec2F px py) (Vec2F vx vy) r

manageShip :: RTree Ship -> Ship -> Int -> IO ()
manageShip store ship i = do
  active <- ActiveShip i <$> newTVarIO ship
  atomically $ upsert active id store

main :: IO ()
main = do
  store <- atomically defaultEmpty 
  flip mapM_ [(1::Int)..100] $ \i -> do
    putStrLn $ "Adding Ship " ++ show i
    ship <- makeRandomShip
    manageShip store ship i
    return ()
  return ()


