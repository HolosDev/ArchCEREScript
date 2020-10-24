module Data.ArchCEREScript.Script where


import Data.ArchCEREScript.Type
import Data.ArchCEREScript.VariablePosition


-------------------------------- # ArchCEREScript # --------------------------------

data ArchCEREScript eis vi vc v vp vt co
  = SSeq
      { aInst :: ArchCERES eis vi vc v vp vt co
      , cNext :: ArchCEREScript eis vi vc v vp vt co
      }
  | SSeqs
      { instList :: [ArchCERES eis vi vc v vp vt co]
      , cNext :: ArchCEREScript eis vi vc v vp vt co
      }
  | SLoop
      { loopCondition :: ArchCEREScript eis vi vc v vp vt co
      , loopScript :: ArchCEREScript eis vi vc v vp vt co
      , cNext :: ArchCEREScript eis vi vc v vp vt co
      }
  | SCase
      -- Note: type of branchCondition should be :: -> Int which is :: (World, SI, Env)
      { branchCondition :: ArchCEREScript eis vi vc v vp vt co
      , -- TODO: Should decide that the result of branchScript would be applied to Env or not
        -- NOTE: If an interpreter applies the result, the programmer should make it carefully
        -- NOTE: If not, an interpreter may calculate same things twice when the result of branchScript is useful
        branchScripts :: VMap (vc eis v vp co) (ArchCEREScript eis vi vc v vp vt co)
      , otherwiseScript :: (ArchCEREScript eis vi vc v vp vt co)
      , cNext :: ArchCEREScript eis vi vc v vp vt co
      }
  | SPar
      { scripts :: [ArchCEREScript eis vi vc v vp vt co]
      , cNext :: ArchCEREScript eis vi vc v vp vt co
      }
  | SEnd
  deriving (Eq, Ord)


-------------------------------- # ArchCERES # --------------------------------
data ArchCERES eis vi vc v vp vt co
  -- | No-Op
  = CRSNoop
  -- | Clear Storage of vp
  | CRSClearVariable    vp
  -- | Initialize Variable at VP@A with Value of VP@B
  | CRSInitVariable     (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co)
  -- | Initialize Variable at Storage of vp with Value of VP@B
  | CRSInitVariableAt   vp                                      (VariablePosition eis vi vc v vp vt co)
  -- | Check existence of Variable at VP@A and store the result to VP@B
  | CRSCheckVariable    (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co)
  -- | Delete Variable at VP@A
  | CRSDeleteVariable   (VariablePosition eis vi vc v vp vt co)
  -- | Modify Value of VP@A by CERESOperator with Value of VP@B
  | CRSModifyValue1     co                                      (VariablePosition eis vi vc v vp vt co)
  -- | Modify Value of VP@A by CERESOperator with Value of VP@B
  | CRSModifyValue2     co                                      (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co)
  -- | Modify Value of VP@A by CERESOperator with Value of VP@B and VP@C
  | CRSModifyValue3     co                                      (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co)
  -- | Modify Value of VP@A by CERESOperator with Value of VP@B, VP@C and VP@D
  | CRSModifyValue4     co                                      (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co)
  -- | Copy Value of VP@B to Variable at VP@A
  | CRSCopyValue        (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co)
  -- | Convert Value of VP@A as like as a given ValueType
  | CRSConvertValue     (VariablePosition eis vi vc v vp vt co) vt
  -- | Convert Value of VP@A as like as ValueType of VP@B
  | CRSConvertValueBy   (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co)
  -- | Convert Value of VP@A with a given rule of VP@B
  | CRSConvertValueWith (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co)
  -- | Replace StrValue of VP@A with indicated Variables in the StrValue
  | CRSReplaceText      (VariablePosition eis vi vc v vp vt co)
  -- | Replace StrValue of VP@A with indicated Variables in the StrValue and store the result to VP@B
  | CRSReplaceTextTo    (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co)
  -- | Replace StrValue of VP@B in StrValue of VP@A as StrValue of VP@C
  | CRSReplaceTextBy    (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co)
  -- | Replace StrValue of VP@B in StrValue of VP@A as StrValue of VP@C and store the result to VP@D
  | CRSReplaceTextByTo  (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co)
  -- | Read & Parse StrValue of VP@A as PtrValue and store the result to VP@B
  | CRSGetPointer       (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co)
  -- | Generate Random Value of VP@A as a ValueType
  | CRSSetPointer       (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co)
  -- | Generate Random Value of VP@A as a given ValueType
  | CRSRandom           (VariablePosition eis vi vc v vp vt co) vt
  -- | Generate Random Value of VP@A as a ValueType of VP@B
  | CRSRandomBy         (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co)
  -- | Generate Random Value of VP@A as a given ValueType with parameters vpC, vpD, and vpE
  | CRSRandomWith       (VariablePosition eis vi vc v vp vt co) vt (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co)
  -- | Generate Random Value of VP@A as a ValueType of VP@B with parameters vpC, vpD, and vpE
  | CRSRandomWithBy     (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co)
  -- | Read & Parse StrValue of VP@A as a script and store the script to VP@B
  | CRSParseScript      (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co)
  -- | Log a content of VP@B to VP@B
  | CRSLog              (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co)
  -- | To0
  | CRSTo0 CHeader
  -- | To1 passes one Value of VP@A
  | CRSTo1 CHeader (VariablePosition eis vi vc v vp vt co)
  -- | To2 passes one Value of VP@A and VP@B
  | CRSTo2 CHeader (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co)
  -- | To3 passes one Value of VP@A, VP@B and VP@C
  | CRSTo3 CHeader (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co)
  -- | To4 passes one Value of VP@A, VP@B, VP@C and VP@D
  | CRSTo4 CHeader (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co)
  -- | To5 passes one Value of VP@A, VP@B, VP@C, VP@D and VP@E
  | CRSTo5 CHeader (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co)
  -- | To6 passes one Value of VP@A, VP@B, VP@C and VP@D, VP@E and VP@F
  | CRSTo6 CHeader (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co)
  -- | To7 passes one Value of VP@A, VP@B, VP@C and VP@D, VP@E, VP@F and VP@G
  | CRSTo7 CHeader (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co)
  -- | To8 passes one Value of VP@A, VP@B, VP@C and VP@D, VP@E, VP@F, VP@G and VP@H
  | CRSTo8 CHeader (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co) (VariablePosition eis vi vc v vp vt co)
  -- | To8 passes one Value of VP@A, VP@B, VP@C and VP@D, VP@E, VP@F, VP@G and VP@H
  | CRSToList CHeader [VariablePosition eis vi vc v vp vt co]
  -- | Ext0
  | CRSExt eis
  deriving (Eq, Ord)
