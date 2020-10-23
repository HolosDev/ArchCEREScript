module Parser.ArchCEREScript.VariablePosition where


import Data.ArchCEREScript.Script
import Data.ArchCEREScript.VariablePosition
import Parser.ArchCEREScript.Type

import Text.Megaparsec
import Text.Megaparsec.Char


parseVariablePositionWith :: Parser vp -> Parser (vi eis vc vp vt co) -> Parser (VariablePosition eis vi vc vp vt co)
parseVariablePositionWith = undefined
