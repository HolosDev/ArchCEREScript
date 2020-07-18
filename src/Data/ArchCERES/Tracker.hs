module Data.ArchCERES.Tracker where


import           Data.ArchCERES.Type
import           Data.ArchCERES.VariablePosition


data Tracker = TrSeq Int Tracker | TrLoop Int Tracker | TrCase Int Tracker | TrEnd
