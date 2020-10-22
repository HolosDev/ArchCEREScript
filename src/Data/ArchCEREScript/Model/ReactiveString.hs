module Data.ArchCEREScript.Model.ReactiveString where


import TextShow as TS

import Data.ArchCEREScript.Model.VariableIndex
import Data.ArchCEREScript.Script
import Data.ArchCEREScript.Script.Show ()
import Data.ArchCEREScript.Show.Util
import Data.ArchCEREScript.Type
import Data.ArchCEREScript.VariablePosition
import Data.ArchCEREScript.VariablePosition.Show ()


-------------------------------- # ReactiveString # --------------------------------

data ReactiveString s vp vc vt co eis
  = RSStr Str (ReactiveString s vp vc vt co eis)
  | RSScr (ArchCEREScript s vp (VariableIndex s vc vt co eis) vc vt co eis) (ReactiveString s vp vc vt co eis)
  | RSVP (VariablePosition vp (VariableIndex s vc vt co eis)) (ReactiveString s vp vc vt co eis)
  | RSEnd

instance (TextShow vp, TextShow vc, TextShow vt, TextShow co, TextShow eis) => Show (ReactiveString s vp vc vt co eis) where
  show = toString . showb

instance (TextShow vp, TextShow vc, TextShow vt, TextShow co, TextShow eis) => TextShow (ReactiveString s vp vc vt co eis) where
  showb (RSStr str rs) = wrapWith "<<<|>" "<|>>>" (fromText str <> semicolon <> showb rs)
  showb (RSScr scr rs) = wrapWith "<<|>>" "<<|>>" (showb scr <> semicolon <> showb rs)
  showb (RSVP vP rs) = wrapWith "<|>>>" "<<<|>" (showb vP <> semicolon <> showb rs)
  showb RSEnd = fromText "<<:>>"
