module Data.ArchCEREScript.Model.ValueContainer where


import Data.IntMap as IM
import Data.Text.Lazy as LT
import Data.Trie.Text as Trie
import Data.Vector as V

import TextShow as TS

import Data.ArchCEREScript.Model.VariableIndex
import Data.ArchCEREScript.Script
import Data.ArchCEREScript.Script.Show
import Data.ArchCEREScript.Show.Util
import Data.ArchCEREScript.Type
import Data.ArchCEREScript.VariablePosition
import Data.ArchCEREScript.VariablePosition.Show ()


-------------------------------- # Value # --------------------------------
-- TODO: Can't determine whether `(ErrValue _) /= (ErrValue _)` or not
data Value s vp vt co eis
  = IntValue {iV :: Int}
  | FltValue {fV :: Double}
  | TxtValue {tV :: Str}
  | BoolValue {bV :: Bool}
  | AtomValue
  | ArrValue {aV :: Array (Value s vp vt co eis)}
  | IMapValue {smV :: IMap (Value s vp vt co eis)}
  | NMapValue {vmV :: NMap (Value s vp vt co eis)}
  | PtrValue {pV :: VariablePosition vp (VariableIndex s (Value s vp vt co eis) vt co eis)}
  | ScrValue {sV :: ArchCEREScript s vp (VariableIndex s (Value s vp vt co eis) vt co eis) (Value s vp vt co eis) vt co eis}
  | RctValue {rVT :: vt, rV :: ArchCEREScript s vp (VariableIndex s (Value s vp vt co eis) vt co eis) (Value s vp vt co eis) vt co eis}
  | RSValue {rsV :: (ReactiveString s vp vt co eis)}
  | ErrValue {errMessage :: Message}

instance (TextShow vp, TextShow vt, TextShow co, TextShow eis) => Show (Value s vp vt co eis) where
  show = toString . showb

instance (TextShow vp, TextShow vt, TextShow co, TextShow eis) => TextShow (Value s vp vt co eis) where
  showb (IntValue i) = fromText "IV" <> wrapDelta (wrapSpace (showb i))
  showb (FltValue f) = fromText "FV" <> wrapDelta (wrapSpace (showb f))
  showb (TxtValue t) = fromText "TV" <> wrapDelta (wrapSpace (showb t))
  showb (BoolValue b) = fromText "BV" <> wrapDelta (wrapSpace (showb b))
  showb AtomValue = fromText "AV" <> wrapDelta (wrapSpace (TS.singleton '-'))
  showb (ArrValue a) = fromText "A" <> wrapDelta (wrapSpace (showbArray a))
   where
    --showbArray :: Array (Value acs vP vt) -> Builder
    showbArray a =
      if V.null a
        then fromText ""
        else V.foldr (\v b -> showb v <> fromText " ||" <> b) (fromText "") a
  showb (IMapValue a) = fromText "IMap" <> (wrapDoubleSquare (showbIMap a))
   where
    --showbIMap :: SMap (Value acs vP vt) -> Builder
    showbIMap im =
      if IM.null im
        then fromText "||  ||"
        else
          IM.foldrWithKey
            (\k v -> (<> space <> showbElem k v <> fromText " ||"))
            (fromText "||")
            im
    --showbElem :: IIdx -> (Value acs vP vt) -> Builder
    showbElem k v = showb k <> colon <> showb v
  showb (NMapValue a) = fromText "NMap[[" <> showbNMap a <> "]]"
   where
    --showbNMap :: NMap (Value acs vP vt) -> Builder
    showbNMap nm =
      if Trie.null nm
        then fromText "||  ||"
        else
          Prelude.foldr
            (\(k, v) -> (<> space <> showbElem k v <> fromText " ||"))
            (fromText "||")
            $ Trie.toList nm
    --showbElem :: Text -> (Value acs vP vt) -> Builder
    showbElem k v = showb k <> colon <> showb v
  showb (PtrValue vP) = fromText "PV" <> wrapDelta (wrapSpace (showb vP))
  showb (ScrValue s) = fromText "SV" <> wrapDelta (wrapSpace (showb s))
  showb (RctValue vt r) = fromText "RV" <> wrapDelta (wrapSpace (showb vt <> space <> showb r))
  showb (RSValue rs) = fromText "RS" <> wrapDelta (wrapSpace (showb rs))
  showb (ErrValue e) = fromText "EV<| " <> fromText e <> fromText " |>"


-------------------------------- # ReactiveString # --------------------------------

data ReactiveString s vp vt co eis
  = RSStr Str (ReactiveString s vp vt co eis)
  | RSScr (ArchCEREScript s vp (VariableIndex s (Value s vp vt co eis) (Value s vp vt co eis) co eis) (Value s vp vt co eis) vt co eis) (ReactiveString s vp vt co eis)
  | RSVP (VariablePosition vp (VariableIndex s (Value s vp vt co eis) vt co eis)) (ReactiveString s vp vt co eis)
  | RSEnd

instance (TextShow vp, TextShow vt, TextShow co, TextShow eis) => Show (ReactiveString s vp vt co eis) where
  show = toString . showb

-- VariablePosition VariablePlace (VariableIndex s (Value s vp vt co eis) vt co eis)

instance (TextShow vp, TextShow vt, TextShow co, TextShow eis) => TextShow (ReactiveString s vp vt co eis) where
  showb (RSStr str rs) = wrapWith "<<<|>" "<|>>>" (fromText str <> semicolon <> showb rs)
  showb (RSScr scr rs) = wrapWith "<<|>>" "<<|>>" (showb scr <> semicolon <> showb rs)
  showb (RSVP vP rs) = wrapWith "<|>>>" "<<<|>" (showb vP <> semicolon <> showb rs)
  showb RSEnd = fromText "<<:>>"
