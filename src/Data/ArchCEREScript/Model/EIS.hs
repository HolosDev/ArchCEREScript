module Data.ArchCEREScript.Model.EIS where


import TextShow


data EIS0 = EIS0

instance Show EIS0 where
  show = toString . showb

instance TextShow EIS0 where
  showb EIS0 = fromText "EIS0"
