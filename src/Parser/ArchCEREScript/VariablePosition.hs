module Parser.ArchCEREScript.VariablePosition where


import Data.ArchCEREScript.Script
import Data.ArchCEREScript.VariablePosition
import Parser.ArchCEREScript.Type

import Text.Megaparsec
import Text.Megaparsec.Char


parseVariablePosition :: Parser vp -> Parser vi -> Parser vc -> Parser (VariablePosition vp vi)
parseVariablePosition = undefined

