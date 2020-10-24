module Data.ArchCEREScript.Model.MCS where


import Data.ArchCEREScript

--import Data.ArchCEREScript.Model.Operator
import Data.ArchCEREScript.Model.Operator
import Data.ArchCEREScript.Model.Value
import Data.ArchCEREScript.Model.ValueContainer
import Data.ArchCEREScript.Model.VariableIndex
import Data.ArchCEREScript.Model.VariablePlace
import Data.ArchCEREScript.Model.VariablePosition


data EIS0 = EIS0

type ModelCS = ArchCEREScript EIS0 VariablePosition VariableIndex ValueContainer Value VariablePlace ValueType CERESOperator
