module Data.ArchCEREScript.Model.Value where


import Data.IntMap as IM
import Data.Maybe
import Data.Text as T
import Data.Trie.Text as Trie
import Data.Vector as V

import TextShow as TS

import Data.ArchCEREScript.Model.ReactiveString
import Data.ArchCEREScript.Model.ValueContainer
import Data.ArchCEREScript.Model.VariableIndex
import Data.ArchCEREScript
import Data.ArchCEREScript.Show ()
import Data.ArchCEREScript.Show.Util
import Data.ArchCEREScript.Type
import Data.ArchCEREScript.Model.VariablePosition


-------------------------------- # Value # --------------------------------
-- TODO: Can't determine whether `(ErrValue _) /= (ErrValue _)` or not
data Value eis vp co
  = IntValue {iV :: Int}
  | FltValue {fV :: Double}
  | TxtValue {tV :: Str}
  | BoolValue {bV :: Bool}
  | AtomValue
  | ArrValue {aV :: Array (Value eis vp co)}
  | IMapValue {imV :: IMap (Value eis vp co)}
  | NMapValue {nmV :: NMap (Value eis vp co)}
  | PtrValue {pV :: VariablePosition eis VariableIndex ValueContainer Value vp ValueType co}
  | ScrValue {sV :: ArchCEREScript eis VariablePosition VariableIndex ValueContainer Value vp ValueType co}
  | RctValue {rVT :: ValueType, rV :: ArchCEREScript eis VariablePosition VariableIndex ValueContainer Value vp ValueType co}
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
  compare (IMapValue imVA) (IMapValue imVB) = compare imVA imVB
  compare (NMapValue nmVA) (NMapValue nmVB) = compare (Trie.toList nmVA) (Trie.toList nmVB)
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
  showb (IntValue iV) = fromText "IV" <> wrapDelta (wrapSpace (showb iV))
  showb (FltValue fV) = fromText "FV" <> wrapDelta (wrapSpace (showb fV))
  showb (TxtValue tV) = fromText "TV" <> wrapDelta (wrapSpace (showb tV))
  showb (BoolValue bV) = fromText "BV" <> wrapDelta (wrapSpace (showb bV))
  showb AtomValue = fromText "AV" <> wrapDelta (wrapSpace (TS.singleton '-'))
  showb (ArrValue aV) = fromText "A" <> wrapDelta (wrapSpace (showbInternalArrayWith aV (fromText " || ")))
  showb (IMapValue imV) = fromText "IMap" <> wrapDoubleSquare (wrapSpace (showbInternalIMapWith imV (fromText " || ") colon))
  showb (NMapValue nmV) = fromText "NMap" <> wrapDoubleSquare (wrapSpace (showbInternalNMapWith nmV (fromText " || ") colon))
  showb (PtrValue vP) = fromText "PV" <> wrapDelta (wrapSpace (showb vP))
  showb (ScrValue sV) = fromText "SV" <> wrapDelta (wrapSpace (showb sV))
  showb (RctValue rVT rV) = fromText "RV" <> wrapDelta (wrapSpace (showb rVT <> space <> showb rV))
  showb (RSValue rsV) = fromText "RS" <> wrapDelta (wrapSpace (showb rsV))
  showb (ErrValue eM) = fromText "EV" <> wrapDelta (wrapSpace (fromText eM))

showRaw :: (TextShow eis, TextShow vp, TextShow co) => Value eis vp co -> String
showRaw = T.unpack . showRawT

showRawT :: (TextShow eis, TextShow vp, TextShow co) => Value eis vp co -> Text
showRawT (IntValue iV) = showt iV
showRawT (FltValue fV) = showt fV
showRawT (TxtValue tV) = tV
showRawT (BoolValue bV) = showt bV
showRawT AtomValue = "Atom"
showRawT (PtrValue vP) = showt vP
showRawT (ArrValue aV) = showt . V.toList $ aV
showRawT (IMapValue _) = error "[Error]<showRawT:=:IMapValue> Not yet implemented"
showRawT (NMapValue _) = error "[Error]<showRawT:=:NMapValue> Not yet implemented"
showRawT (ScrValue sV) = showt sV
showRawT (RctValue _ rV) = showt rV
showRawT (RSValue rsV) = showt rsV
showRawT (ErrValue eM) = eM


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
