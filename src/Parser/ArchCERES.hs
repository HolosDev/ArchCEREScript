module Parser.ArchCERES where


import Data.ArchCERES.Script
import Parser.ArchCERES.Type

import Text.Megaparsec

parseArchCEREScript :: Parser vp -> Parser (vi vc) -> Parser vc -> Parser vt -> Parser co -> Parser eis -> Parser (ArchCEREScript s vp vi vc vt co eis)
parseArchCEREScript parseVP parseVI parseVC parseVT parseCO parseEIS = return SEnd
