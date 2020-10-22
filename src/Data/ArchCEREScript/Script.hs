module Data.ArchCEREScript.Script where


import Data.ArchCEREScript.Type
import Data.ArchCEREScript.VariablePosition


-------------------------------- # ArchCEREScript # --------------------------------

data ArchCEREScript s vp vi vc vt co eis
  = SSeq
    { aInst :: ArchCERES vp vi vc vt co eis
    , cNext :: ArchCEREScript s vp vi vc vt co eis
    }
  | SSeqs
    { instList :: [ArchCERES vp vi vc vt co eis]
    , cNext :: ArchCEREScript s vp vi vc vt co eis
    }
  | SLoop
    { loopCondition :: ArchCEREScript s vp vi vc vt co eis
    , loopScript :: ArchCEREScript s vp vi vc vt co eis
    , cNext :: ArchCEREScript s vp vi vc vt co eis
    }
  | SCase
    -- Note: type of branchCondition should be :: s -> Int which s is :: (World, SI, Env)
    { branchCondition :: ArchCEREScript s vp vi vc vt co eis
    -- TODO: Should decide that the result of branchScript would be applied to Env or not
    -- NOTE: If an interpreter applies the result, the programmer should make it carefully
    -- NOTE: If not, an interpreter may calculate same things twice when the result of branchScript is useful
    , branchScripts :: VMap vc (ArchCEREScript s vp vi vc vt co eis)
    , otherwiseScript :: (ArchCEREScript s vp vi vc vt co eis)
    , cNext :: ArchCEREScript s vp vi vc vt co eis
    }
  | SPar
    { scripts :: [ArchCEREScript s vp vi vc vt co eis]
    , cNext :: ArchCEREScript s vp vi vc vt co eis
    }
  | SEnd


-------------------------------- # ArchCERES # --------------------------------
data ArchCERES vp vi vc vt co eis
  -- | No-Op
  = CRSNoop
  -- | Clear Storage of vp
  | CRSClearVariable    vp
  -- | Initialize Variable at VP@A with Value of VP@B
  | CRSInitVariable     (VariablePosition vp vi) (VariablePosition vp vi)
  -- | Initialize Variable at Storage of vp with Value of VP@B
  | CRSInitVariableAt   vp                       (VariablePosition vp vi)
  -- | Check existence of Variable at VP@A and store the result to VP@B
  | CRSCheckVariable    (VariablePosition vp vi) (VariablePosition vp vi)
  -- | Delete Variable at VP@A
  | CRSDeleteVariable   (VariablePosition vp vi)
  -- | Modify Value of VP@A by CERESOperator with Value of VP@B
  | CRSModifyValue1     co                       (VariablePosition vp vi)
  -- | Modify Value of VP@A by CERESOperator with Value of VP@B
  | CRSModifyValue2     co                       (VariablePosition vp vi) (VariablePosition vp vi)
  -- | Modify Value of VP@A by CERESOperator with Value of VP@B and VP@C
  | CRSModifyValue3     co                       (VariablePosition vp vi) (VariablePosition vp vi) (VariablePosition vp vi)
  -- | Modify Value of VP@A by CERESOperator with Value of VP@B, VP@C and VP@D
  | CRSModifyValue4     co                       (VariablePosition vp vi) (VariablePosition vp vi) (VariablePosition vp vi) (VariablePosition vp vi)
  -- | Copy Value of VP@B to Variable at VP@A
  | CRSCopyValue        (VariablePosition vp vi) (VariablePosition vp vi)
  -- | Convert Value of VP@A as like as a given ValueType
  | CRSConvertValue     (VariablePosition vp vi) vt
  -- | Convert Value of VP@A as like as ValueType of VP@B
  | CRSConvertValueBy   (VariablePosition vp vi) (VariablePosition vp vi)
  -- | Convert Value of VP@A with a given rule of VP@B
  | CRSConvertValueWith (VariablePosition vp vi) (VariablePosition vp vi)
  -- | Replace StrValue of VP@A with indicated Variables in the StrValue
  | CRSReplaceText      (VariablePosition vp vi)
  -- | Replace StrValue of VP@A with indicated Variables in the StrValue and store the result to VP@B
  | CRSReplaceTextTo    (VariablePosition vp vi) (VariablePosition vp vi)
  -- | Replace StrValue of VP@B in StrValue of VP@A as StrValue of VP@C
  | CRSReplaceTextBy    (VariablePosition vp vi) (VariablePosition vp vi) (VariablePosition vp vi)
  -- | Replace StrValue of VP@B in StrValue of VP@A as StrValue of VP@C and store the result to VP@D
  | CRSReplaceTextByTo  (VariablePosition vp vi) (VariablePosition vp vi) (VariablePosition vp vi) (VariablePosition vp vi)
  -- | Read & Parse StrValue of VP@A as PtrValue and store the result to VP@B
  | CRSGetPointer       (VariablePosition vp vi) (VariablePosition vp vi)
  -- | Generate Random Value of VP@A as a ValueType
  | CRSSetPointer       (VariablePosition vp vi) (VariablePosition vp vi)
  -- | Generate Random Value of VP@A as a given ValueType
  | CRSRandom           (VariablePosition vp vi) vt
  -- | Generate Random Value of VP@A as a ValueType of VP@B
  | CRSRandomBy         (VariablePosition vp vi) (VariablePosition vp vi)
  -- | Generate Random Value of VP@A as a given ValueType with parameters vpC, vpD, and vpE
  | CRSRandomWith       (VariablePosition vp vi) vt (VariablePosition vp vi) (VariablePosition vp vi) (VariablePosition vp vi)
  -- | Generate Random Value of VP@A as a ValueType of VP@B with parameters vpC, vpD, and vpE
  | CRSRandomWithBy     (VariablePosition vp vi) (VariablePosition vp vi) (VariablePosition vp vi) (VariablePosition vp vi) (VariablePosition vp vi)
  -- | Read & Parse StrValue of VP@A as a script and store the script to VP@B
  | CRSParseScript      (VariablePosition vp vi) (VariablePosition vp vi)
  -- | Log a content of VP@B to VP@B
  | CRSLog              (VariablePosition vp vi) (VariablePosition vp vi)
  -- | To0
  | CRSTo0 CHeader
  -- | To1 passes one Value of VP@A
  | CRSTo1 CHeader (VariablePosition vp vi)
  -- | To2 passes one Value of VP@A and VP@B
  | CRSTo2 CHeader (VariablePosition vp vi) (VariablePosition vp vi)
  -- | To3 passes one Value of VP@A, VP@B and VP@C
  | CRSTo3 CHeader (VariablePosition vp vi) (VariablePosition vp vi) (VariablePosition vp vi)
  -- | To4 passes one Value of VP@A, VP@B, VP@C and VP@D
  | CRSTo4 CHeader (VariablePosition vp vi) (VariablePosition vp vi) (VariablePosition vp vi) (VariablePosition vp vi)
  -- | To5 passes one Value of VP@A, VP@B, VP@C, VP@D and VP@E
  | CRSTo5 CHeader (VariablePosition vp vi) (VariablePosition vp vi) (VariablePosition vp vi) (VariablePosition vp vi) (VariablePosition vp vi)
  -- | To6 passes one Value of VP@A, VP@B, VP@C and VP@D, VP@E and VP@F
  | CRSTo6 CHeader (VariablePosition vp vi) (VariablePosition vp vi) (VariablePosition vp vi) (VariablePosition vp vi) (VariablePosition vp vi) (VariablePosition vp vi)
  -- | To7 passes one Value of VP@A, VP@B, VP@C and VP@D, VP@E, VP@F and VP@G
  | CRSTo7 CHeader (VariablePosition vp vi) (VariablePosition vp vi) (VariablePosition vp vi) (VariablePosition vp vi) (VariablePosition vp vi) (VariablePosition vp vi) (VariablePosition vp vi)
  -- | To8 passes one Value of VP@A, VP@B, VP@C and VP@D, VP@E, VP@F, VP@G and VP@H
  | CRSTo8 CHeader (VariablePosition vp vi) (VariablePosition vp vi) (VariablePosition vp vi) (VariablePosition vp vi) (VariablePosition vp vi) (VariablePosition vp vi) (VariablePosition vp vi) (VariablePosition vp vi)
  -- | To8 passes one Value of VP@A, VP@B, VP@C and VP@D, VP@E, VP@F, VP@G and VP@H
  | CRSToList CHeader [VariablePosition vp vi]
  -- | Ext0
  | CRSExt eis
  deriving (Eq, Ord)
