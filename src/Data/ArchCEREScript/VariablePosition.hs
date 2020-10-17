module Data.ArchCEREScript.VariablePosition where


import           Data.ArchCEREScript.Type


---------------- # VariablePosition # ----------------

-- NOTE: This fixes shape of VariablePosition and VariableIndex
data VariablePosition vp vi vc = VP vp (vi vc) deriving (Eq, Ord)
