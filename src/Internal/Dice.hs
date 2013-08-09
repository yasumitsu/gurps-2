{-|
Module : Internal.Dice

Utilities for rolling 6-sided dice.
-}

module Internal.Dice
  ( rollDice
  , roll3d
    -- * Re-exports from "System.Random.MWC"
  , GenIO
  , withSystemRandom
  ) where

import System.Random.MWC

------------------------------------------------------------------------

rollDice :: Int -> Int -> GenIO -> IO Int
rollDice d adds = uniformR (d * 1 + adds, d * 6 + adds)

roll3d :: GenIO -> IO Int
roll3d = rollDice 3 0
