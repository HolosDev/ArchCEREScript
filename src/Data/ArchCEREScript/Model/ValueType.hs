module Data.ArchCEREScript.Model.ValueType where


import TextShow


-------------------------------- ValueType --------------------------------

data ValueType
  = VTInt
  | VTDbl
  | VTStr
  | VTBool
  | VTAtom
  | VTArr
  | VTPtr
  | VTScr
  | VTRct
  | VTErr
  deriving (Eq, Ord, Enum)

instance Show ValueType where
  show = toString . showb

instance TextShow ValueType where
  showb VTInt = fromText "C-Int"
  showb VTDbl = fromText "C-Dbl"
  showb VTStr = fromText "C-Str"
  showb VTBool = fromText "CBool"
  showb VTAtom = fromText "CAtom"
  showb VTArr = fromText "C-Arr"
  showb VTPtr = fromText "C-Ptr"
  showb VTScr = fromText "C-Scr"
  showb VTRct = fromText "C-Rct"
  showb VTErr = fromText "C-Err"
