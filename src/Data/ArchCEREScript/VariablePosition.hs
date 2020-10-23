module Data.ArchCEREScript.VariablePosition where


---------------- # VariablePosition # ----------------

-- NOTE: This fixes shape of VariablePosition and VariableIndex
data VariablePosition eis vi vc vp vt co = VP vp (vi eis vc vp vt co) deriving (Eq, Ord)
