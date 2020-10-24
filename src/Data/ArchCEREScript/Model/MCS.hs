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
type MCS = ArchCERES EIS0 VariablePosition VariableIndex ValueContainer Value VariablePlace ValueType CERESOperator
type MVPosition = VariablePosition EIS0 VariableIndex ValueContainer Value VariablePlace ValueType CERESOperator
type MVIndex = VariableIndex EIS0 ValueContainer Value VariablePlace ValueType CERESOperator
type MVContainer = ValueContainer EIS0 Value VariablePlace CERESOperator
type MV = Value EIS0 VariablePlace CERESOperator
type MVPlace = VariablePlace
type MVType = ValueType
type MCSOp = CERESOperator
