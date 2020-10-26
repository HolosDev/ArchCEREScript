module Data.ArchCEREScript.Tracker.Show where


import TextShow

import Data.ArchCEREScript.Tracker
import Data.ArchCEREScript.Type
import Data.ArchCEREScript.Show.Util


instance (TextShow eis, TextShow (v eis vp co), TextShow vp, TextShow co) => Show (Tracker v eis vp co) where
  show = toString . showb

instance (TextShow eis, TextShow (v eis vp co), TextShow vp, TextShow co) => TextShow (Tracker v eis vp co) where
  showb (TrSeq i trc) = fromText "TrSeq" <> wrapDelta (showb i) <> showb trc
  showb (TrCase v trc) = fromText "TrCase" <> wrapDelta (showb v) <> showb trc
  showb (TrPar pars trc) = fromText "TrPar" <> wrapDelta (wrapDoubleSquare (showbInternalIMapWith pars comma bar)) <> showb trc
  showb TrEnd = fromText "TrEnd"
