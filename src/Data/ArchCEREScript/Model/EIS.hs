{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.ArchCEREScript.Model.EIS where


import TextShow

import Data.ArchCEREScript.Model.Operator
import Data.ArchCEREScript.Model.Value
import Data.ArchCEREScript.Model.ValueContainer
import Data.ArchCEREScript.Model.VariableIndex
import Data.ArchCEREScript.Model.VariablePlace
import Data.ArchCEREScript.Model.VariablePosition


data NEIS

type NMVariablePosition = VariablePosition NEIS VariableIndex ValueContainer Value VariablePlace ValueType CERESOperator
type NMVariableIndex = VariableIndex NEIS ValueContainer Value VariablePlace ValueType CERESOperator
type NMValueContainer = ValueContainer NEIS Value VariablePlace CERESOperator
type NMValue = Value NEIS VariablePlace CERESOperator

data family MEIS vP vi vc v vp vt co
data instance MEIS NMVariablePosition NMVariableIndex NMValueContainer NMValue VariablePlace ValueType CERESOperator = MEIS0
type MEIS0 = MEIS NMVariablePosition NMVariableIndex NMValueContainer NMValue VariablePlace ValueType CERESOperator

instance Show (MEIS NMVariablePosition NMVariableIndex NMValueContainer NMValue VariablePlace ValueType CERESOperator) where
  show = toString . showb

instance TextShow (MEIS NMVariablePosition NMVariableIndex NMValueContainer NMValue VariablePlace ValueType CERESOperator) where
  showb MEIS0 = fromText "MEIS0"
