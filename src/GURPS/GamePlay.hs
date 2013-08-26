{-|
Module : GURPS.GamePlay

Types and functions for simulating GURPS gameplay.

All rules are from the GURPS 4th Lite book.

GURPS Lite is (C) by Steve Jackson Games Inc., and may be downloaded at
no cost from <www.sjgames.com/gurps/lite/>.
-}

module GURPS.GamePlay
  (
    -- * Success rolls
    module GURPS.GamePlay.Success
    -- * Reaction rolls
  , module GURPS.GamePlay.Reaction
    -- * Damage rolls
  , module GURPS.GamePlay.Damage
    -- * Contests
  , module GURPS.GamePlay.Contest
  ) where

import GURPS.GamePlay.Success
import GURPS.GamePlay.Reaction
import GURPS.GamePlay.Damage
import GURPS.GamePlay.Contest
