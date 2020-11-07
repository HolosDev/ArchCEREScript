{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.ArchCEREScript.Model.EIS where


import TextShow

import Data.ArchCEREScript
import Data.ArchCEREScript.Model.Operator
import Data.ArchCEREScript.Model.Value
import Data.ArchCEREScript.Model.ValueContainer
import Data.ArchCEREScript.Model.VariableIndex
import Data.ArchCEREScript.Model.VariablePlace
import Data.ArchCEREScript.Model.VariablePosition


type ModelCS = ArchCEREScript MEIS VariablePosition VariableIndex ValueContainer Value VariablePlace ValueType CERESOperator
type MCS = ArchCERES MEIS VariablePosition VariableIndex ValueContainer Value VariablePlace ValueType CERESOperator
type MVPosition = VariablePosition MEIS VariableIndex ValueContainer Value VariablePlace ValueType CERESOperator
type MVIndex = VariableIndex MEIS ValueContainer Value VariablePlace ValueType CERESOperator
type MVContainer = ValueContainer MEIS Value VariablePlace CERESOperator
type MV = Value MEIS VariablePlace CERESOperator


data MEIS
  = MEIS0
  | MEIS1 {rVP1 :: VariablePosition MEIS VariableIndex ValueContainer Value VariablePlace ValueType CERESOperator}

getRVP1 :: ArchCERES MEIS VariablePosition VariableIndex ValueContainer Value VariablePlace ValueType CERESOperator -> VariablePosition MEIS VariableIndex ValueContainer Value VariablePlace ValueType CERESOperator
getRVP1 (CRSExt eis) = rVP1 (eis :: MEIS)
getRVP1 acs = rVP1 (acs :: ArchCERES MEIS VariablePosition VariableIndex ValueContainer Value VariablePlace ValueType CERESOperator)

instance Show MEIS where
  show = toString . showb

instance TextShow MEIS where
  showb MEIS0 = fromText "MEIS0"
