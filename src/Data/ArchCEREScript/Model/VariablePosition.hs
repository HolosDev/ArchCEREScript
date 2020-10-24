module Data.ArchCEREScript.Model.VariablePosition where


import TextShow

import Data.ArchCEREScript.Show.Util


---------------- # VariablePosition # ----------------

-- NOTE: This fixes shape of VariablePosition and VariableIndex
data VariablePosition eis vi vc v vp vt co = VP vp (vi eis vc v vp vt co) deriving (Eq, Ord)


instance (TextShow vp, TextShow (vi eis vc v vp vt co)) => Show (VariablePosition eis vi vc v vp vt co) where
  show = toString . showb

instance (TextShow vp, TextShow (vi eis vc v vp vt co)) => TextShow (VariablePosition eis vi vc v vp vt co) where
  showb (VP vPlace vIndex) = showb vPlace <> (wrapSquare (showb vIndex))
