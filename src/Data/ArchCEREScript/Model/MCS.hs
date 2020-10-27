module Data.ArchCEREScript.Model.MCS
  ( module Data.ArchCEREScript.Model.MCS
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


type ModelCS = ArchCEREScript MEIS0 VariablePosition VariableIndex ValueContainer Value VariablePlace ValueType CERESOperator
type MCS = ArchCERES MEIS0 VariablePosition VariableIndex ValueContainer Value VariablePlace ValueType CERESOperator
type MVPosition = VariablePosition MEIS0 VariableIndex ValueContainer Value VariablePlace ValueType CERESOperator
type MVIndex = VariableIndex MEIS0 ValueContainer Value VariablePlace ValueType CERESOperator
type MVContainer = ValueContainer MEIS0 Value VariablePlace CERESOperator
type MV = Value MEIS0 VariablePlace CERESOperator
type MVPlace = VariablePlace
type MVType = ValueType
type MCSOp = CERESOperator
