{-|
Module : GamePlay

Types and functions for simulating GURPS gameplay.

All rules are from the GURPS 4th Lite book.
-}

module GamePlay
  (
    -- * Success rolls
    -- $success
    SuccessRoll(..)
  , successRoll
    -- * Reaction rolls
  , Reaction(..)
  , reactionRoll
    -- * Damage rolls
    -- $damage
  , damageRoll
  ,
  ) where

import Data.Ord

import System.Random.MWC

------------------------------------------------------------------------

rollDice :: Int -> Int -> GenIO -> IO Int
rollDice d adds = uniformR (d * 1 + adds, d * 6 + adds)

roll3d :: GenIO -> IO Int
roll3d = rollDice 3 0

------------------------------------------------------------------------
-- $success

-- | The result of a success roll.
data SuccessRoll = SuccessRoll
  { succeeds :: Bool
    -- ^ Did the roll succeed?
  , critical :: Bool
    -- ^ A critical success or failure?
  , margin :: Int
    -- ^ Absolute margin.
    --
    -- For failure, smaller is better; for success larger is better.
  } deriving (Eq, Show)

instance Ord SuccessRoll where
  compare s1 s2 =
    case comparing succeeds s1 s2 of
      EQ | succeeds s1 -> comparing margin s1 s2
         | otherwise   -> comparing margin s2 s1
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

------------------------------------------------------------------------
-- $reaction

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

------------------------------------------------------------------------
-- $damage

-- | Perform a damage roll using the \"dice+adds\" system.
damageRoll :: Int -> Int -> GenIO -> IO Int
damageRoll d adds g = rollDice d adds g
