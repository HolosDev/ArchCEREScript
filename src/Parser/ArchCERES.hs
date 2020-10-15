module Parser.ArchCERES where

import Parser.ArchCERES.Type
import Data.ArchCERES.Script

import Text.Megaparsec

parseA :: ScriptSource -> ArchCEREScript s vp vi vc vt co eis
parseA _ = SEnd
