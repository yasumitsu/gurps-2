{-|
Module : GURPS.SkillTable

Types and functions for working with skill tables.

All skills are from GURPS 4th Lite.

GURPS Lite is (C) Steve Jackson Games, Inc.
-}

module GURPS.SkillTable
  ( SkillEntry(..)
  , SkillTable
  , defaultSkillTable
  , lookupSkill
  , lookupSkillCost
  , mkCostTable
  ) where

import qualified Data.List as L

------------------------------------------------------------------------

data SkillEntry = SE
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
  [
    SE "Acting"                "IQ" "A" True  ["IQ-5"]
  , SE "Area Knowledge"        "IQ" "E" True  ["IQ-4"]
  , SE "Camouflage"            "IQ" "E" True  ["IQ-4"]
  , SE "Computer Operation"    "IQ" "E" True  ["IQ-4"]
  , SE "Computer Programming"  "IQ" "H" True  []
  , SE "Diagnosis"             "IQ" "H" True  ["IQ-6"]
  , SE "Disguise"              "IQ" "A" True  ["IQ-5"]
  , SE "Electronics Operation" "IQ" "A" True  ["IQ-5"]
  , SE "Electronics Repair"    "IQ" "A" True  ["IQ-5"]
  , SE "Engineer"              "IQ" "H" True  []
  , SE "Diplomacy"             "IQ" "H" False ["IQ-6"]
  , SE "Intimidation"          "IQ" "A" False ["IQ-5"]
  , SE "Sex Appeal"            "HT" "A" False ["HT-3"]
  , SE "Interrogation"         "IQ" "A" False ["IQ-5"]
  , SE "Jumping"               "DX" "E" False []
  , SE "Karate"                "DX" "H" False []
  , SE "Leadership"            "IQ" "A" False ["IQ-5"]
  , SE "Mathematics"           "IQ" "H" True  ["IQ-6"]
  , SE "Merchant"              "IQ" "A" False ["IQ-5"]
  , SE "Observation"           "IQ" "A" False ["IQ-5"]
  , SE "Research"              "IQ" "A" True  ["IQ-5"]
  , SE "Lockpicking"           "IQ" "A" False ["IQ-5"]
  , SE "Tactics"               "IQ" "H" False ["IQ-6"]
  , SE "Guns (Pistol)"         "DX" "E" True  ["DX-4"]
  , SE "Guns (Rifle)"          "DX" "E" True  ["DX-4"]
  , SE "Scrounging"            "IQ" "E" False ["IQ-4"]
  , SE "Shadowing"             "IQ" "A" False ["IQ-5"]
  , SE "Holdout"               "IQ" "A" False ["IQ-5"]
  , SE "Search"                "IQ" "A" False ["IQ-5"]
  , SE "First Aid"             "IQ" "E" True  ["IQ-4"]
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
