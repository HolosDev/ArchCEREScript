module Parser.ArchCEREScript.Model.VariablePosition where


import Data.ArchCEREScript
import Parser.ArchCEREScript.Type

import Text.Megaparsec
import Text.Megaparsec.Char


parseVariablePositionWith :: Parser vp -> Parser (vi eis vc v vp vt co) -> Parser (vP eis vi vc v vp vt co)
parseVariablePositionWith = undefined
