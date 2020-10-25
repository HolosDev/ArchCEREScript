module Parser.ArchCEREScript.Type where


import Text.Megaparsec (Parsec)
import Data.Void (Void)
import Data.Text.Lazy (Text)

import Data.ArchCEREScript ()


type ScriptSource = Text
type Parser = Parsec Void ScriptSource
