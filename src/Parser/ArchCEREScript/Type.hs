module Parser.ArchCEREScript.Type where


import Text.Megaparsec (Parsec)
import Data.Void (Void)
import Data.Text (Text)

import Data.ArchCEREScript.Script ()


type ScriptSource = Text
type Parser = Parsec Void ScriptSource
