module Data.ArchCEREScript.ModelCS where


import Data.ArchCEREScript.Script
import Data.ArchCEREScript.VariablePosition

--import Data.ArchCEREScript.Model.Operator
import Data.ArchCEREScript.Model.Operator
import Data.ArchCEREScript.Model.Value
import Data.ArchCEREScript.Model.ValueContainer
import Data.ArchCEREScript.Model.VariableIndex
import Data.ArchCEREScript.Model.VariablePlace


type ModelCS = ArchCEREScript () VariableIndex ValueContainer Value VariablePlace ValueType CERESOperator
