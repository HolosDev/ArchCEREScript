module Data.ArchCEREScript.Model.VariablePlace where


import TextShow


-------------------------------- # VariablePlace # --------------------------------

data VariablePlace
  = AtTricky
  | AtPtr
  | AtWorld
  | AtTime
  | AtNWorld
  | AtNTime
  | AtDict
  | AtNDict
  | AtVars
  | AtNVars
  | AtLVars
  | AtLNVars
  | AtLTemp
  | AtLNTemp
  | AtIRct
  | AtRct
  | AtReg
  | AtHere
  | AtReturn
  | AtResult
  | AtNull
  deriving (Eq, Ord, Enum, Bounded)

instance Show VariablePlace where
  show = toString . showb

instance TextShow VariablePlace where
  showb AtTricky = fromText "AtTricky"
  showb AtPtr = fromText "AtPtr"
  showb AtWorld = fromText "AtWorld"
  showb AtTime = fromText "AtTime"
  showb AtNWorld = fromText "AtNWorld"
  showb AtNTime = fromText "AtNTime"
  showb AtDict = fromText "AtDict"
  showb AtNDict = fromText "AtNDict"
  showb AtVars = fromText "AtVars"
  showb AtNVars = fromText "AtNVars"
  showb AtLVars = fromText "AtLVars"
  showb AtLNVars = fromText "AtLNVars"
  showb AtLTemp = fromText "AtLTemp"
  showb AtLNTemp = fromText "AtLNTemp"
  showb AtIRct = fromText "AtIRct"
  showb AtRct = fromText "AtRct"
  showb AtReg = fromText "AtReg"
  showb AtHere = fromText "AtHere"
  showb AtReturn = fromText "AtReturn"
  showb AtResult = fromText "AtResult"
  showb AtNull = fromText "AtNull"
