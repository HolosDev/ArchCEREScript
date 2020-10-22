module Data.ArchCEREScript.VariablePosition where


---------------- # VariablePosition # ----------------

-- NOTE: This fixes shape of VariablePosition and VariableIndex
data VariablePosition vp vi = VP vp vi deriving (Eq, Ord)
