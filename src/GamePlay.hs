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
    -- $success
    SuccessRoll(..)
  , successRoll
    -- * Reaction rolls
    -- $reaction
  , Reaction(..)
  , reactionRoll
    -- * Damage rolls
    -- $damage
  , damageRoll
    -- * Contests
    -- $contests
  , quickContest
  , regularContest
  ) where

import Internal.Dice

import Data.Ord

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
  } deriving (Eq, Show)

instance Ord SuccessRoll where
  compare s1 s2 =
    case comparing succeeds s1 s2 of
      EQ | succeeds s1 -> comparing margin s1 s2 -- success: larger better
         | otherwise   -> comparing margin s2 s1 -- failure: smaller better
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

------------------------------------------------------------------------
-- $contests

-- | @quickContest t1 t2@ runs a Quick contest (Lite, p. 3) between
-- two players, comparing their respective Effective skill t1 and t2.
--
-- The result indicates the winner (1 or 2) and the winning roll, if
-- a winner could be determined.
quickContest :: Int -> Int -> GenIO -> IO (Maybe (Int, SuccessRoll))
quickContest t1 t2 g = do
  r1 <- successRoll t1 g
  r2 <- successRoll t2 g
  return $ case compare r1 r2 of
    GT -> Just (1, r1)
    LT -> Just (2, r2)
    EQ -> Nothing

-- | @regularContest t1 t2@ runs a Regular contest (Lite, p. 3)
-- between two players, comparing their respective Effective skill
-- t1 and t2.
--
-- The result indicates the winner (1 or 2), the winning roll, and the
-- number of rounds it took to determine a winner.
regularContest :: Int -> Int -> GenIO -> IO (Int, SuccessRoll, Int)
regularContest t1 t2 g = go 1
  where
    go rounds = do
      r1 <- successRoll t1 g
      r2 <- successRoll t2 g
      case winner r1 r2 of
        Just (i, r) -> return (i, r, rounds)
        Nothing     -> go (succ rounds)

    winner r1 r2
      | succeeds r1 && not (succeeds r2) = Just (1, r1)
      | not (succeeds r1) && succeeds r2 = Just (2, r2)
      | otherwise                        = Nothing
