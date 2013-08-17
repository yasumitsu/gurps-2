module GamePlay.Contest
  ( quickContest
  , regularContest
  ) where

import Internal.Dice
import GamePlay.Success

------------------------------------------------------------------------

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
