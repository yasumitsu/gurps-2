module Internal.Table (Table, showTable) where

import qualified Data.Map as M

------------------------------------------------------------------------

type Table = M.Map String

------------------------------------------------------------------------

showTable :: Show v => String -> M.Map String v -> String
showTable hdr m = unlines $ hdr : (map f $ M.toList m)
  where
    f (k, v) = replicate 4 ' ' ++ k ++ " = " ++ show v
