module Data.ArchCEREScript.Model.MCS where


import Data.ArchCEREScript.Script

--import Data.ArchCEREScript.Model.Operator
import Data.ArchCEREScript.Model.Operator
import Data.ArchCEREScript.Model.Value
import Data.ArchCEREScript.Model.ValueContainer
import Data.ArchCEREScript.Model.VariableIndex
import Data.ArchCEREScript.Model.VariablePlace
import Data.ArchCEREScript.Model.VariablePosition


type ModelCS = ArchCEREScript () VariablePosition VariableIndex ValueContainer Value VariablePlace ValueType CERESOperator
