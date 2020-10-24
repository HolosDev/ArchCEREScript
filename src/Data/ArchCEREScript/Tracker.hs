module Data.ArchCEREScript.Tracker where


import Data.ArchCEREScript.Type


data Tracker = TrSeq Int Tracker | TrLoop Int Tracker | TrCase Int Tracker | TrPar (SMap Tracker) Tracker | TrEnd
