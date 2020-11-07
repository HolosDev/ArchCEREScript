module Data.ArchCEREScript where


import Data.ArchCEREScript.Type


-------------------------------- # ArchCEREScript # --------------------------------

data ArchCEREScript eis vP vi vc v vp vt co
  = SSeq
      { aInst :: ArchCERES eis vP vi vc v vp vt co
      , cNext :: ArchCEREScript eis vP vi vc v vp vt co
      }
  | SSeqs
      { instList :: [ArchCERES eis vP vi vc v vp vt co]
      , cNext :: ArchCEREScript eis vP vi vc v vp vt co
      }
  | SLoop
      { loopCondition :: ArchCEREScript eis vP vi vc v vp vt co
      , loopScript :: ArchCEREScript eis vP vi vc v vp vt co
      , loopLabel :: Maybe LoopLabel
      , cNext :: ArchCEREScript eis vP vi vc v vp vt co
      }
  | SCase
      -- Note: type of branchCondition should be :: -> Int which is :: (World, SI, Env)
      { branchCondition :: ArchCEREScript eis vP vi vc v vp vt co
      , -- TODO: Should decide that the result of branchScript would be applied to Env or not
        -- NOTE: If an interpreter applies the result, the programmer should make it carefully
        -- NOTE: If not, an interpreter may calculate same things twice when the result of branchScript is useful
        branchScripts :: VMap (v eis vp co) (ArchCEREScript eis vP vi vc v vp vt co)
      , otherwiseScript :: (ArchCEREScript eis vP vi vc v vp vt co)
      , cNext :: ArchCEREScript eis vP vi vc v vp vt co
      }
  | SPar
      { scripts :: [ArchCEREScript eis vP vi vc v vp vt co]
      , cNext :: ArchCEREScript eis vP vi vc v vp vt co
      }
  | SEnd
  deriving (Eq, Ord)


-------------------------------- # ArchCERES # --------------------------------
data ArchCERES eis vP vi vc v vp vt co
  = -- | No-Op
    CRSNoop
  | -- | Run Script at rVP1
    CRSRun
      {rVP1 :: vP eis vi vc v vp vt co}
  | -- | Return a content of rVP1 to wVP1
    CRSReturn
      {rVP1 :: vP eis vi vc v vp vt co, wVP1 :: vP eis vi vc v vp vt co}
  | -- | Claims error with a content of rVP1 to wVP1
    CRSError
      {operator :: co, rVP1 :: vP eis vi vc v vp vt co, wVP1 :: vP eis vi vc v vp vt co}
  | -- | Break
    CRSBreak
      {breakLabel :: Maybe LoopLabel}
  | -- | Clear Storage of vp
    CRSClearVariables
      {variablePlace :: vp}
  | -- | Initialize Variable at wVP1
    CRSInitVariable
      {wVP1 :: vP eis vi vc v vp vt co}
  | -- | Initialize Variable at Storage of vp with Value of rVP1
    CRSInitVariableAt
      {variablePlace :: vp, wVP1 :: vP eis vi vc v vp vt co}
  | -- | Initialize Variable at wVP1 with Value of rVP1
    CRSInitVariableBy
      {wVP1 :: vP eis vi vc v vp vt co, rVP1 :: vP eis vi vc v vp vt co}
  | -- | Check existence of Variable at rVP1
    CRSCheckVariable
      {rVP1 :: vP eis vi vc v vp vt co}
  | -- | Delete Variable at wVP1
    CRSDeleteVariable
      {wVP1 :: vP eis vi vc v vp vt co}
  | -- | Modify no Value, but do CERESOperator
    CRSDo
      {operator :: co}
  | -- | Modify Value of wVP1 by CERESOperator with Value of rVP1
    CRSModifyValue
      {operator :: co, wVP1 :: vP eis vi vc v vp vt co}
  | -- | Modify Value of wVP1 by CERESOperator with Value of rVP1
    CRSModifyValue1
      {operator :: co, wVP1 :: vP eis vi vc v vp vt co, rVP1 :: vP eis vi vc v vp vt co}
  | -- | Modify Value of wVP1 by CERESOperator with Value of rVP1 and rVP2
    CRSModifyValue2
      {operator :: co, wVP1 :: vP eis vi vc v vp vt co, rVP1 :: vP eis vi vc v vp vt co, rVP2 :: vP eis vi vc v vp vt co}
  | -- | Modify Value of wVP1 by CERESOperator with Value of rVP1, rVP1 and rVP3
    CRSModifyValue3
      {operator :: co, wVP1 :: vP eis vi vc v vp vt co, rVP1 :: vP eis vi vc v vp vt co, rVP2 :: vP eis vi vc v vp vt co, rVP3 :: vP eis vi vc v vp vt co}
  | -- | Modify Value of wVP1 by CERESOperator with Value of rVP1, rVP1, rVP3 and rVP4
    CRSModifyValue4
      {operator :: co, wVP1 :: vP eis vi vc v vp vt co, rVP1 :: vP eis vi vc v vp vt co, rVP2 :: vP eis vi vc v vp vt co, rVP3 :: vP eis vi vc v vp vt co, rVP4 :: vP eis vi vc v vp vt co}
  | -- | Modify Value of wVP1 and wVP2 by CERESOperator
    CRSModifyBothValue
      {operator :: co, wVP1 :: vP eis vi vc v vp vt co, wVP2 :: vP eis vi vc v vp vt co}
  | -- | Modify Value of wVP1 and wVP2 by CERESOperator with Value of rVP1
    CRSModifyBothValue1
      {operator :: co, wVP1 :: vP eis vi vc v vp vt co, wVP2 :: vP eis vi vc v vp vt co, rVP1 :: vP eis vi vc v vp vt co}
  | -- | Modify Value of wVP1 and wVP2 by CERESOperator with Value of rVP1 and rVP2
    CRSModifyBothValue2
      {operator :: co, wVP1 :: vP eis vi vc v vp vt co, wVP2 :: vP eis vi vc v vp vt co, rVP1 :: vP eis vi vc v vp vt co, rVP2 :: vP eis vi vc v vp vt co}
  | -- | Modify Value of wVP1 and wVP2 by CERESOperator with Value of rVP1, rVP2 and rVP3
    CRSModifyBothValue3
      {operator :: co, wVP1 :: vP eis vi vc v vp vt co, wVP2 :: vP eis vi vc v vp vt co, rVP1 :: vP eis vi vc v vp vt co, rVP2 :: vP eis vi vc v vp vt co, rVP3 :: vP eis vi vc v vp vt co}
  | -- | Modify Value of wVP1 by CERESOperator with Value of rVP1, rVP1, rVP3 and rVP4
    CRSModifyBothValue4
      {operator :: co, wVP1 :: vP eis vi vc v vp vt co, wVP2 :: vP eis vi vc v vp vt co, rVP1 :: vP eis vi vc v vp vt co, rVP2 :: vP eis vi vc v vp vt co, rVP3 :: vP eis vi vc v vp vt co, rVP4 :: vP eis vi vc v vp vt co}
  | -- | Modify Values of wVPs by CERESOperator with rVPs
    CRSModifyValues
      {operator :: co, wVPs :: Array (vP eis vi vc v vp vt co), rVPs :: Array (vP eis vi vc v vp vt co)}
  | -- | Copy Value of rVP1 to Variable at wVP1
    CRSCopyValue
      {rVP1 :: vP eis vi vc v vp vt co, wVP1 :: vP eis vi vc v vp vt co}
  | -- | Convert Value of wVP1 as like as a given ValueType
    CRSConvertValue
      {wVP1 :: vP eis vi vc v vp vt co, vType :: vt}
  | -- | Generate Random Value at wVP1 as a given ValueType
    CRSRandom
      {wVP1 :: vP eis vi vc v vp vt co, vType :: vt}
  | -- | Log a content of rVP1 to wVP1
    CRSLog
      {rVP1 :: vP eis vi vc v vp vt co, wVP1 :: vP eis vi vc v vp vt co}
  | -- | Ext
    CRSExt {eis :: eis}
  deriving (Eq, Ord)
