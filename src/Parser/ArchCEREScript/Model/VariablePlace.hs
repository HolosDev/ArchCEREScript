module Parser.ArchCEREScript.Model.VariablePlace where


import Text.Megaparsec
import Text.Megaparsec.Char

import Data.ArchCEREScript.Model.VariablePlace
import Parser.ArchCEREScript.Type


parseVariablePlace :: Parser VariablePlace
parseVariablePlace = choice
  [ AtTricky <$ string "AtTricky"
  , AtPtr <$ string "AtPtr"
  , AtWorld <$ string "AtWorld"
  , AtTime <$ string "AtTime"
  , AtNWorld <$ string "AtNWorld"
  , AtNTime <$ string "AtNTime"
  , AtDict <$ string "AtDict"
  , AtNDict <$ string "AtNDict"
  , AtVars <$ string "AtVars"
  , AtNVars <$ string "AtNVars"
  , AtLVars <$ string "AtLVars"
  , AtLNVars <$ string "AtLNVars"
  , AtLTemp <$ string "AtLTemp"
  , AtLNTemp <$ string "AtLNTemp"
  , AtIRct <$ string "AtIRct"
  , AtRct <$ string "AtRct"
  , AtReg <$ string "AtReg"
  , AtHere <$ string "AtHere"
  , AtNull <$ string "AtNull"
  ]
