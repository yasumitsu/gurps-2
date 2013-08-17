{-|
Module : GamePlay

Types and functions for simulating GURPS gameplay.

All rules are from the GURPS 4th Lite book.

GURPS Lite is (C) by Steve Jackson Games Inc., and may be downloaded at
no cost from <www.sjgames.com/gurps/lite/>.
-}

module GamePlay
  (
    -- * Success rolls
    module GamePlay.Success
    -- * Reaction rolls
  , module GamePlay.Reaction
    -- * Damage rolls
  , module GamePlay.Damage
    -- * Contests
  , module GamePlay.Contest
  ) where

import GamePlay.Success
import GamePlay.Reaction
import GamePlay.Damage
import GamePlay.Contest
