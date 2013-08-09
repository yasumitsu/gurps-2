{-|
Module : SkillTable

Types and functions for working with skill tables.

All skills are from GURPS 4th Lite.

GURPS Lite is (C) Steve Jackson Games, Inc.
-}

module SkillTable
  ( SkillEntry(..)
  , SkillTable
  , defaultSkillTable
  , lookupSkill
  , lookupSkillCost
  , mkCostTable
  ) where

import qualified Data.List as L

------------------------------------------------------------------------

data SkillEntry = SkillEntry
  { eSkillName :: String
  , eSkillAttr :: String
  , eSkillDifficulty :: String
  , eSkillTech :: Bool
  , eSkillDefaults :: [String]
  } deriving Show

type SkillTable = [ SkillEntry ]

------------------------------------------------------------------------

defaultSkillTable :: SkillTable
defaultSkillTable =
  [ SkillEntry "Computer Operation" "IQ" "E" True ["IQ-4"]
  , SkillEntry "Computer Programming" "IQ" "H" True []
  , SkillEntry "Jumping" "DX" "E" False []
  , SkillEntry "Lockpicking" "IQ" "A" False ["IQ-5"]
  , SkillEntry "Tactics" "IQ" "H" False ["IQ-6"]
  ]

------------------------------------------------------------------------

lookupSkill :: String -> SkillTable -> Maybe SkillEntry
lookupSkill name tbl = L.find ((==) name . eSkillName) tbl

------------------------------------------------------------------------

-- | Consult the Skill Cost Table.
--
-- @lookupSkillCost (newLevel - attrLevel) diff@ returns the cost of
-- buying a skill level @newLevel@ relative to the @attrLevel@ of the
-- skill's controlling attribute and its difficulty @diff@.
lookupSkillCost :: Int -> String -> Maybe Int
lookupSkillCost r d
  | r < minLevel d = Nothing
  | otherwise      = lookup r $ mkCostTable d

-- | Generate a complete Skill Cost Table.
--
-- See GURPS Lite, p. 13.
mkCostTable :: String -> [ (Int, Int) ]
mkCostTable d = zip [minLevel d ..] (1 : 2 : [4, 8..])
-- Note: this implementation exploits the fact that skill costs are
-- computed the same for all difficulty levels, the only difference being
-- their minimum level.

minLevel :: String -> Int
minLevel "E" = 0
minLevel "A" = (-1)
minLevel "H" = (-2)
minLevel _   = error "minLevel: bad difficulty"
