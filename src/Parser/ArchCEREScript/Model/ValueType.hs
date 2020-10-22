module Parser.ArchCEREScript.Model.ValueType where


import Text.Megaparsec
import Text.Megaparsec.Char

import Data.ArchCEREScript.Model.ValueType
import Parser.ArchCEREScript.Type


-------------------------------- ValueType --------------------------------

parseValueType :: Parser ValueType
parseValueType = choice
  [ VTInt <$ string "VT-Int"
  , VTDbl <$ string "VT-Dbl"
  , VTStr <$ string "VT-Str"
  , VTBool <$ string "VTBool"
  , VTAtom <$ string "VTAtom"
  , VTArr <$ string "VT-Arr"
  , VTPtr <$ string "VT-Ptr"
  , VTScr <$ string "VT-Scr"
  , VTRct <$ string "VT-Rct"
  , VTErr <$ string "VT-Err"
  ]
