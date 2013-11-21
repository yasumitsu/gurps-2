module GURPS.GamePlay.Damage
  ( damageRoll
  ) where

import Internal.Dice

------------------------------------------------------------------------

-- | Perform a damage roll using the \"dice+adds\" system.
damageRoll :: Int -> Int -> GenIO -> IO Int
damageRoll d adds g = rollDice d adds g
