module Data.ArchCEREScript.Script where


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
      , cNext :: ArchCEREScript eis vP vi vc v vp vt co
      }
  | SCase
      -- Note: type of branchCondition should be :: -> Int which is :: (World, SI, Env)
      { branchCondition :: ArchCEREScript eis vP vi vc v vp vt co
      , -- TODO: Should decide that the result of branchScript would be applied to Env or not
        -- NOTE: If an interpreter applies the result, the programmer should make it carefully
        -- NOTE: If not, an interpreter may calculate same things twice when the result of branchScript is useful
        branchScripts :: VMap (vc eis v vp co) (ArchCEREScript eis vP vi vc v vp vt co)
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
  -- | No-Op
  = CRSNoop
  -- | Clear Storage of vp
  | CRSClearVariable    vp
  -- | Initialize Variable at VP@A with Value of VP@B
  | CRSInitVariable     (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co)
  -- | Initialize Variable at Storage of vp with Value of VP@B
  | CRSInitVariableAt   vp                        (vP eis vi vc v vp vt co)
  -- | Check existence of Variable at VP@A and store the result to VP@B
  | CRSCheckVariable    (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co)
  -- | Delete Variable at VP@A
  | CRSDeleteVariable   (vP eis vi vc v vp vt co)
  -- | Modify Value of VP@A by CERESOperator with Value of VP@B
  | CRSModifyValue1     co                        (vP eis vi vc v vp vt co)
  -- | Modify Value of VP@A by CERESOperator with Value of VP@B
  | CRSModifyValue2     co                        (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co)
  -- | Modify Value of VP@A by CERESOperator with Value of VP@B and VP@C
  | CRSModifyValue3     co                        (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co)
  -- | Modify Value of VP@A by CERESOperator with Value of VP@B, VP@C and VP@D
  | CRSModifyValue4     co                        (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co)
  -- | Copy Value of VP@B to Variable at VP@A
  | CRSCopyValue        (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co)
  -- | Convert Value of VP@A as like as a given ValueType
  | CRSConvertValue     (vP eis vi vc v vp vt co) vt
  -- | Convert Value of VP@A as like as ValueType of VP@B
  | CRSConvertValueBy   (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co)
  -- | Convert Value of VP@A with a given rule of VP@B
  | CRSConvertValueWith (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co)
  -- | Replace StrValue of VP@A with indicated Variables in the StrValue
  | CRSReplaceText      (vP eis vi vc v vp vt co)
  -- | Replace StrValue of VP@A with indicated Variables in the StrValue and store the result to VP@B
  | CRSReplaceTextTo    (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co)
  -- | Replace StrValue of VP@B in StrValue of VP@A as StrValue of VP@C
  | CRSReplaceTextBy    (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co)
  -- | Replace StrValue of VP@B in StrValue of VP@A as StrValue of VP@C and store the result to VP@D
  | CRSReplaceTextByTo  (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co)
  -- | Read & Parse StrValue of VP@A as PtrValue and store the result to VP@B
  | CRSGetPointer       (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co)
  -- | Generate Random Value of VP@A as a ValueType
  | CRSSetPointer       (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co)
  -- | Generate Random Value of VP@A as a given ValueType
  | CRSRandom           (vP eis vi vc v vp vt co) vt
  -- | Generate Random Value of VP@A as a ValueType of VP@B
  | CRSRandomBy         (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co)
  -- | Generate Random Value of VP@A as a given ValueType with parameters vpC, vpD, and vpE
  | CRSRandomWith       (vP eis vi vc v vp vt co) vt (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co)
  -- | Generate Random Value of VP@A as a ValueType of VP@B with parameters vpC, vpD, and vpE
  | CRSRandomWithBy     (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co)
  -- | Read & Parse StrValue of VP@A as a script and store the script to VP@B
  | CRSParseScript      (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co)
  -- | Log a content of VP@B to VP@B
  | CRSLog              (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co)
  -- | To0
  | CRSTo0 CHeader
  -- | To1 passes one Value of VP@A
  | CRSTo1 CHeader (vP eis vi vc v vp vt co)
  -- | To2 passes one Value of VP@A and VP@B
  | CRSTo2 CHeader (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co)
  -- | To3 passes one Value of VP@A, VP@B and VP@C
  | CRSTo3 CHeader (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co)
  -- | To4 passes one Value of VP@A, VP@B, VP@C and VP@D
  | CRSTo4 CHeader (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co)
  -- | To5 passes one Value of VP@A, VP@B, VP@C, VP@D and VP@E
  | CRSTo5 CHeader (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co)
  -- | To6 passes one Value of VP@A, VP@B, VP@C and VP@D, VP@E and VP@F
  | CRSTo6 CHeader (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co)
  -- | To7 passes one Value of VP@A, VP@B, VP@C and VP@D, VP@E, VP@F and VP@G
  | CRSTo7 CHeader (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co)
  -- | To8 passes one Value of VP@A, VP@B, VP@C and VP@D, VP@E, VP@F, VP@G and VP@H
  | CRSTo8 CHeader (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co) (vP eis vi vc v vp vt co)
  -- | To8 passes one Value of VP@A, VP@B, VP@C and VP@D, VP@E, VP@F, VP@G and VP@H
  | CRSToList CHeader [vP eis vi vc v vp vt co]
  -- | Ext0
  | CRSExt eis
  deriving (Eq, Ord)
