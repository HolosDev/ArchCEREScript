module Parser.ArchCERES.Type where


import Text.Megaparsec (Parsec)
import Data.Void (Void)
import Data.Text (Text)

import Data.ArchCERES.Script ()


type ScriptSource = Text
type Parser = Parsec Void ScriptSource
