module Data.ArchCEREScript.Type.Show where


import TextShow

import Data.ArchCEREScript.Type


instance Show Atom where
  show Atom = "Atom"

instance TextShow Atom where
  showb Atom = fromLazyText "Atom"
