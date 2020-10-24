module Data.ArchCEREScript.VariablePosition where


---------------- # VariablePosition # ----------------

-- NOTE: This fixes shape of VariablePosition and VariableIndex
data VariablePosition eis vi vc v vp vt co = VP vp (vi eis vc v vp vt co) deriving (Eq, Ord)
