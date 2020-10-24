module Data.ArchCEREScript.VariablePosition.Show where


import TextShow

import Data.ArchCEREScript.Show.Util
import Data.ArchCEREScript.VariablePosition


instance (TextShow vp, TextShow (vi eis vc v vp vt co)) => Show (VariablePosition eis vi vc v vp vt co) where
  show = toString . showb

instance (TextShow vp, TextShow (vi eis vc v vp vt co)) => TextShow (VariablePosition eis vi vc v vp vt co) where
  showb (VP vPlace vIndex) = showb vPlace <> (wrapSquare (showb vIndex))
