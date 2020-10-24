module Parser.ArchCEREScript.Model.Value where


import Text.Megaparsec
import Text.Megaparsec.Char

import Data.ArchCEREScript.Model.Value
import Parser.ArchCEREScript.Type


-------------------------------- ValueType --------------------------------

parseValueType :: Parser ValueType
parseValueType = choice
  [ VTInt <$ string "VT-Int"
  , VTFlt <$ string "VT-Dbl"
  , VTText <$ string "VTText"
  , VTBool <$ string "VTBool"
  , VTAtom <$ string "VTAtom"
  , VTArr <$ string "VT-Arr"
  , VTIMap <$ string "VTIMap"
  , VTNMap <$ string "VTNMap"
  , VTPtr <$ string "VT-Ptr"
  , VTScr <$ string "VT-Scr"
  , VTRct <$ string "VT-Rct"
  , VTRStr <$ string "VTRStr"
  , VTErr <$ string "VT-Err"
  ]
