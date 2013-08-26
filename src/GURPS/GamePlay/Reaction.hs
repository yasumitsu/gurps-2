module GURPS.GamePlay.Reaction
  ( Reaction(..)
  , reactionRoll
  ) where

import Internal.Dice

------------------------------------------------------------------------

-- | Reaction roll results; consult the table in Lite, p. 3.
data Reaction
  = Disaster
  | VeryBad
  | Bad
  | Poor
  | Neutral
  | Good
  | VeryGood
  | Excellent
    deriving (Eq, Ord, Show)

------------------------------------------------------------------------

-- | Perform a reaction roll, with the given modifiers.
reactionRoll :: Int -> GenIO -> IO Reaction
reactionRoll mods g = test_reaction `fmap` rollDice 3 mods g

test_reaction :: Int -> Reaction
test_reaction r
  | r < 1     = Disaster
  | r <= 3    = VeryBad
  | r <= 6    = Bad
  | r <= 9    = Poor
  | r <= 12   = Neutral
  | r <= 15   = Good
  | r <= 18   = VeryGood
  | otherwise = Excellent -- 19+
