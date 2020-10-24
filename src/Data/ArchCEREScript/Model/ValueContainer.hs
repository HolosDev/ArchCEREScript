module Data.ArchCEREScript.Model.ValueContainer where


import Data.IntMap as IM
import Data.Trie.Text as Trie
import Data.Vector as V

import TextShow as TS

import Data.ArchCEREScript.Model.VariableIndex
import Data.ArchCEREScript.Model.ReactiveString
import Data.ArchCEREScript.Script
import Data.ArchCEREScript.Script.Show ()
import Data.ArchCEREScript.Show.Util
import Data.ArchCEREScript.Type
import Data.ArchCEREScript.VariablePosition
import Data.ArchCEREScript.VariablePosition.Show ()


-------------------------------- # Value # --------------------------------
-- TODO: Can't determine whether `(ErrValue _) /= (ErrValue _)` or not
data Value eis vp vt co
  = IntValue {iV :: Int}
  | FltValue {fV :: Double}
  | TxtValue {tV :: Str}
  | BoolValue {bV :: Bool}
  | AtomValue
  | ArrValue {aV :: Array (Value eis vp vt co)}
  | IMapValue {smV :: IMap (Value eis vp vt co)}
  | NMapValue {vmV :: NMap (Value eis vp vt co)}
  | PtrValue {pV :: VariablePosition eis VariableIndex Value vp vt co}
  | ScrValue {sV :: ArchCEREScript eis VariableIndex Value vp vt co}
  | RctValue {rVT :: vt, rV :: ArchCEREScript eis VariableIndex Value vp vt co}
  | RSValue {rsV :: ReactiveString eis Value vp vt co}
  | ErrValue {errMessage :: Message}
  deriving (Eq)

instance (Ord eis, Ord vp, Ord vt, Ord co) => Ord (Value eis vp vt co) where
  compare (IntValue iVA) (IntValue iVB) = compare iVA iVB
  compare (FltValue fVA) (FltValue fVB) = compare fVA fVB
  compare (TxtValue tVA) (TxtValue tVB) = compare tVA tVB
  compare (BoolValue bVA) (BoolValue bVB) = compare bVA bVB
  compare AtomValue AtomValue = EQ
  compare (ArrValue aVA) (ArrValue aVB) = compare aVA aVB
  compare (IMapValue smVA) (IMapValue smVB) = compare smVA smVB
  compare (NMapValue vmVA) (NMapValue vmVB) = compare (Trie.toList vmVA) (Trie.toList vmVB)
  compare (PtrValue pVA) (PtrValue pVB) = compare pVA pVB
  compare (ScrValue sVA) (ScrValue sVB) = compare sVA sVB
  compare (RctValue rVTA rVA) (RctValue rVTB rVB) = if compare rVTA rVTB /= EQ then compare rVA rVB else EQ
  compare (RSValue rsVA) (RSValue rsVB) = compare rsVA rsVB
  compare (ErrValue eVA) (ErrValue eVB) = compare eVA eVB
  --TODO: For ordering, vt should be coupled with Value, and we can use vc for container
  --compare vA vB = compare (valueType vA) (valueType vB)

instance (TextShow eis, TextShow vp, TextShow vt, TextShow co) => Show (Value eis vp vt co) where
  show = toString . showb

instance (TextShow eis, TextShow vp, TextShow vt, TextShow co) => TextShow (Value eis vp vt co) where
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
