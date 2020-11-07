module Data.ArchCEREScript.Model.MCS
  ( module Data.ArchCEREScript
  , module Data.ArchCEREScript.Model.MCS
  , module Data.ArchCEREScript.Model.EIS
  , module Data.ArchCEREScript.Model.VariablePosition
  , module Data.ArchCEREScript.Model.VariableIndex
  , module Data.ArchCEREScript.Model.ValueContainer
  , module Data.ArchCEREScript.Model.Value
  , module Data.ArchCEREScript.Model.VariablePlace
  , module Data.ArchCEREScript.Model.Operator
) where


import Data.ArchCEREScript

import Data.ArchCEREScript.Model.EIS
import Data.ArchCEREScript.Model.Operator
import Data.ArchCEREScript.Model.Value
import Data.ArchCEREScript.Model.ValueContainer
import Data.ArchCEREScript.Model.VariableIndex
import Data.ArchCEREScript.Model.VariablePlace
import Data.ArchCEREScript.Model.VariablePosition


type MVPlace = VariablePlace
type MVType = ValueType
type MCSOp = CERESOperator
