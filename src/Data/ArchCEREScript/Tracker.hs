module Data.ArchCEREScript.Tracker where


import Data.ArchCEREScript.Type


data Tracker v eis vp co
  = TrSeq Int (Tracker v eis vp co)
  | TrCase (v eis vp co) (Tracker v eis vp co)
  | TrPar (SMap (Tracker v eis vp co)) (Tracker v eis vp co)
  | TrEnd
