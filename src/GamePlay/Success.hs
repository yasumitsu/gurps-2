module GamePlay.Success
  (
    SuccessRoll(..)
  , successRoll
  ) where

import Internal.Dice
import Data.Ord

------------------------------------------------------------------------

-- | The result of a success roll.
data SuccessRoll = SuccessRoll
  { succeeds :: Bool
    -- ^ Did the roll succeed?
  , critical :: Bool
    -- ^ A critical success or failure?
  , margin :: Int
    -- ^ Absolute margin.
  } deriving (Eq, Show)

instance Ord SuccessRoll where
  compare s1 s2 =
    case comparing succeeds s1 s2 of
      EQ | succeeds s1 -> comparing margin s1 s2 -- larger better
         | otherwise   -> comparing margin s2 s1 -- smaller better
      o  -> o

------------------------------------------------------------------------

-- | Perform a single success roll against some target number, typically
-- representing the Effective skill.
successRoll :: Int -> GenIO -> IO SuccessRoll
successRoll target g = do
  r <- roll3d g
  let (suc, crit) = test_success r target
  return $ SuccessRoll suc crit (abs (target - r))

test_success :: Int -> Int -> (Bool, Bool) -- ^ succeeds, critical
test_success 3 _  = (True, True)
test_success 4 _  = (True, True)
test_success 5 t
  | t >= 15 = (True, True)
test_success 6 t
  | t >= 16 = (True, True)
test_success 18 _ = (False, True)
test_success 17 t = (False, t <= 15)
test_success r t
  | r - t >= 10 = (False, True)
test_success r t = (r <= t, False)
