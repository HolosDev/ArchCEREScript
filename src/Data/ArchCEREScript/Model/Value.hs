module Data.ArchCEREScript.Model.Value where


import Data.IntMap as IM
import Data.Text as T
import Data.Trie.Text as Trie
import Data.Vector as V

import TextShow as TS

import Data.ArchCEREScript.Model.ReactiveString
import Data.ArchCEREScript.Model.ValueContainer
import Data.ArchCEREScript.Model.VariableIndex
import Data.ArchCEREScript.Script
import Data.ArchCEREScript.Script.Show ()
import Data.ArchCEREScript.Show.Util
import Data.ArchCEREScript.Type
import Data.ArchCEREScript.VariablePosition
import Data.ArchCEREScript.VariablePosition.Show ()


-------------------------------- # Value # --------------------------------
-- TODO: Can't determine whether `(ErrValue _) /= (ErrValue _)` or not
data Value eis vp co
  = IntValue {iV :: Int}
  | FltValue {fV :: Double}
  | TxtValue {tV :: Str}
  | BoolValue {bV :: Bool}
  | AtomValue
  | ArrValue {aV :: Array (Value eis vp co)}
  | IMapValue {smV :: IMap (Value eis vp co)}
  | NMapValue {vmV :: NMap (Value eis vp co)}
  | PtrValue {pV :: VariablePosition eis VariableIndex ValueContainer Value vp ValueType co}
  | ScrValue {sV :: ArchCEREScript eis VariableIndex ValueContainer Value vp ValueType co}
  | RctValue {rVT :: ValueType, rV :: ArchCEREScript eis VariableIndex ValueContainer Value vp ValueType co}
  | RSValue {rsV :: ReactiveString eis ValueContainer Value vp ValueType co}
  | ErrValue {errMessage :: Message}
  deriving (Eq)

instance (Ord eis, Ord vp, Ord co) => Ord (Value eis vp co) where
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
  --TODO: For ordering, v vt should be coupled with Value, and we can use vc for container
  --compare vA vB = compare (valueType vA) (valueType vB)

instance (TextShow eis, TextShow vp, TextShow co) => Show (Value eis vp co) where
  show = toString . showb

instance (TextShow eis, TextShow vp, TextShow co) => TextShow (Value eis vp co) where
  showb (IntValue i) = fromText "IV" <> wrapDelta (wrapSpace (showb i))
  showb (FltValue f) = fromText "FV" <> wrapDelta (wrapSpace (showb f))
  showb (TxtValue t) = fromText "TV" <> wrapDelta (wrapSpace (showb t))
  showb (BoolValue b) = fromText "BV" <> wrapDelta (wrapSpace (showb b))
  showb AtomValue = fromText "AV" <> wrapDelta (wrapSpace (TS.singleton '-'))
  showb (ArrValue a) = fromText "A" <> wrapDelta (wrapSpace (showbArray a))
   where
    --showbArray :: Array (Value acs vP v vt) -> Builder
    showbArray a =
      if V.null a
        then fromText ""
        else V.foldr (\v b -> showb v <> fromText " ||" <> b) (fromText "") a
  showb (IMapValue a) = fromText "IMap" <> (wrapDoubleSquare (showbIMap a))
   where
    --showbIMap :: SMap (Value acs vP v vt) -> Builder
    showbIMap im =
      if IM.null im
        then fromText "||  ||"
        else
          IM.foldrWithKey
            (\k v -> (<> space <> showbElem k v <> fromText " ||"))
            (fromText "||")
            im
    --showbElem :: IIdx -> (Value acs vP v vt) -> Builder
    showbElem k v = showb k <> colon <> showb v
  showb (NMapValue a) = fromText "NMap[[" <> showbNMap a <> "]]"
   where
    --showbNMap :: NMap (Value acs vP v vt) -> Builder
    showbNMap nm =
      if Trie.null nm
        then fromText "||  ||"
        else
          Prelude.foldr
            (\(k, v) -> (<> space <> showbElem k v <> fromText " ||"))
            (fromText "||")
            $ Trie.toList nm
    --showbElem :: Text -> (Value acs vP v vt) -> Builder
    showbElem k v = showb k <> colon <> showb v
  showb (PtrValue vP) = fromText "PV" <> wrapDelta (wrapSpace (showb vP))
  showb (ScrValue s) = fromText "SV" <> wrapDelta (wrapSpace (showb s))
  showb (RctValue vt r) = fromText "RV" <> wrapDelta (wrapSpace (showb vt <> space <> showb r))
  showb (RSValue rs) = fromText "RS" <> wrapDelta (wrapSpace (showb rs))
  showb (ErrValue e) = fromText "EV<| " <> fromText e <> fromText " |>"

showRaw :: (TextShow eis, TextShow vp, TextShow co) => Value eis vp co -> String
showRaw = T.unpack . showRawT

showRawT :: (TextShow eis, TextShow vp, TextShow co) => Value eis vp co -> Text
showRawT (IntValue i) = showt i
showRawT (FltValue f) = showt f
showRawT (TxtValue t) = t
showRawT (BoolValue b) = showt b
showRawT AtomValue = "Atom"
showRawT (PtrValue vp) = showt vp
showRawT (ArrValue a) = showt . V.toList $ a
showRawT (IMapValue _) = error "[Error]<showRawT:=:IMapValue> Not yet implemented"
showRawT (NMapValue _) = error "[Error]<showRawT:=:NMapValue> Not yet implemented"
showRawT (ScrValue c) = showt c
showRawT (RctValue _ c) = showt c
showRawT (RSValue rs) = showt rs
showRawT (ErrValue e) = e


-------------------------------- ValueType --------------------------------

data ValueType
  = VTInt
  | VTFlt
  | VTText
  | VTBool
  | VTAtom
  | VTArr
  | VTIMap
  | VTNMap
  | VTPtr
  | VTScr
  | VTRct
  | VTRStr
  | VTErr
  deriving (Eq, Ord, Enum)

instance Show ValueType where
  show = toString . showb

instance TextShow ValueType where
  showb VTInt = fromText "C-Int"
  showb VTFlt = fromText "C-Flt"
  showb VTText = fromText "CText"
  showb VTBool = fromText "CBool"
  showb VTAtom = fromText "CAtom"
  showb VTArr = fromText "C-Arr"
  showb VTIMap = fromText "CIMap"
  showb VTNMap = fromText "CNMap"
  showb VTPtr = fromText "C-Ptr"
  showb VTScr = fromText "C-Scr"
  showb VTRct = fromText "C-Rct"
  showb VTRStr = fromText "CRStr"
  showb VTErr = fromText "C-Err"
