module Data.ArchCEREScript.Model.ReactiveString where


import TextShow as TS

import Data.ArchCEREScript.Model.VariableIndex
import Data.ArchCEREScript.Script
import Data.ArchCEREScript.Script.Show ()
import Data.ArchCEREScript.Show.Util
import Data.ArchCEREScript.Type
import Data.ArchCEREScript.Model.VariablePosition


-------------------------------- # ReactiveString # --------------------------------
data ReactiveString eis vc v vp vt co
  = RSStr Str (ReactiveString eis vc v vp vt co)
  | RSScr (ArchCEREScript eis VariablePosition VariableIndex vc v vp vt co) (ReactiveString eis vc v vp vt co)
  | RSVP (VariablePosition eis VariableIndex vc v vp vt co) (ReactiveString eis vc v vp vt co)
  | RSEnd
  deriving (Eq, Ord)

instance (TextShow eis, TextShow (vc eis v vp co), TextShow (v eis vp co), TextShow vp, TextShow vt, TextShow co) => Show (ReactiveString eis vc v vp vt co) where
  show = toString . showb

instance (TextShow eis, TextShow (vc eis v vp co), TextShow (v eis vp co), TextShow vp, TextShow vt, TextShow co) => TextShow (ReactiveString eis vc v vp vt co) where
  showb (RSStr str rs) = wrapWith "<<<|>" "<|>>>" (fromText str <> semicolon <> showb rs)
  showb (RSScr scr rs) = wrapWith "<<|>>" "<<|>>" (showb scr <> semicolon <> showb rs)
  showb (RSVP vP rs) = wrapWith "<|>>>" "<<<|>" (showb vP <> semicolon <> showb rs)
  showb RSEnd = fromText "<<:>>"
