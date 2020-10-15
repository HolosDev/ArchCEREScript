module Parser.ArchCERES.Type where


import Data.ArchCERES.Script ()

import Text.Megaparsec (Parsec)
import Data.Void (Void)
import Data.Text (Text)


type ScriptSource = Text
type Parser = Parsec Void ScriptSource
