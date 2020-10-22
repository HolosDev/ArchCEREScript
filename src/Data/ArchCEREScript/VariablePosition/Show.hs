module Data.ArchCEREScript.VariablePosition.Show where


import TextShow

import Data.ArchCEREScript.Show.Util
import Data.ArchCEREScript.VariablePosition


instance (TextShow vp, TextShow vi) => Show (VariablePosition vp vi) where
  show = toString . showb

instance (TextShow vp, TextShow vi) => TextShow (VariablePosition vp vi) where
  showb (VP vPlace vIndex) = showb vPlace <> (wrapSquare (showb vIndex))
