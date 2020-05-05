module Data.ArchCERES.Script where


import           Data.IntMap                    ( IntMap )


import           Data.ArchCERES.Type
import           Data.ArchCERES.VariablePosition


---------------- # ArchCEREScript # ----------------

data ArchCEREScript s vp vi v vt co eis
  = SCase
    -- Note: type of branchCondition should be :: s -> Int which s is :: (World, SI, Env)
    { branchCondition :: ArchCEREScript s vp vi v vt co eis
    -- TODO: Should decide that the result of branchScript would be applied to Env or not
    -- NOTE: If an interpreter applies the result, the programmer should make it carefully
    -- NOTE: If not, an interpreter may calculate same things twice when the result of branchScript is useful
    , branchScripts :: IntMap (ArchCEREScript s vp vi v vt co eis)
    , cNext :: ArchCEREScript s vp vi v vt co eis
    }
  | SSeq
    { aInst :: ArchCERES vp vi v vt co eis
    , cNext :: ArchCEREScript s vp vi v vt co eis
    }
  | SEnd

data ArchCERES vp vi v vt co eis
  -- | No-Op
  = CRSNoop
  -- | Initialize Variable at VP@A with Value of VP@B
  | CRSInitVariable     (VariablePosition vp vi v) (VariablePosition vp vi v)
  -- | Check existence of Variable at VP@A and store the result to VP@B
  | CRSCheckVariable    (VariablePosition vp vi v) (VariablePosition vp vi v)
  -- | Delete Variable at VP@A
  | CRSDeleteVariable   (VariablePosition vp vi v)
  -- | Modify Value of VP@A by CERESOperator with Value of VP@B
  | CRSModifyValue1     (VariablePosition vp vi v) co
  -- | Modify Value of VP@A by CERESOperator with Value of VP@B
  | CRSModifyValue2     (VariablePosition vp vi v) (VariablePosition vp vi v) co
  -- | Modify Value of VP@A by CERESOperator with Value of VP@B and VP@C
  | CRSModifyValue3     (VariablePosition vp vi v) (VariablePosition vp vi v) (VariablePosition vp vi v) co
  -- | Modify Value of VP@A by CERESOperator with Value of VP@B, VP@C and VP@D
  | CRSModifyValue4     (VariablePosition vp vi v) (VariablePosition vp vi v) (VariablePosition vp vi v) (VariablePosition vp vi v) co
  -- | Copy Value of VP@B to Variable at VP@A
  | CRSCopyValue        (VariablePosition vp vi v) (VariablePosition vp vi v)
  -- | Convert Value of VP@A as like as a given ValueType
  | CRSConvertValue     (VariablePosition vp vi v) vt
  -- | Convert Value of VP@A as like as ValueType of VP@B
  | CRSConvertValueBy   (VariablePosition vp vi v) (VariablePosition vp vi v)
  -- | Convert Value of VP@A with a given rule of VP@B
  | CRSConvertValueWith (VariablePosition vp vi v) (VariablePosition vp vi v)
  -- | Replace StrValue of VP@A with indicated Variables in the StrValue
  | CRSReplaceText      (VariablePosition vp vi v)
  -- | Replace StrValue of VP@A with indicated Variables in the StrValue and store the result to VP@B
  | CRSReplaceTextTo    (VariablePosition vp vi v) (VariablePosition vp vi v)
  -- | Replace StrValue of VP@B in StrValue of VP@A as StrValue of VP@C
  | CRSReplaceTextBy    (VariablePosition vp vi v) (VariablePosition vp vi v) (VariablePosition vp vi v)
  -- | Replace StrValue of VP@B in StrValue of VP@A as StrValue of VP@C and store the result to VP@D
  | CRSReplaceTextByTo  (VariablePosition vp vi v) (VariablePosition vp vi v) (VariablePosition vp vi v) (VariablePosition vp vi v)
  -- | Read & Parse StrValue of VP@A as PtrValue and store the result to VP@B
  | CRSGetPointer       (VariablePosition vp vi v) (VariablePosition vp vi v)
  -- | Generate Random Value of VP@A as a ValueType
  | CRSSetPointer       (VariablePosition vp vi v) (VariablePosition vp vi v)
  -- | Generate Random Value of VP@A as a given ValueType
  | CRSRandom           (VariablePosition vp vi v) vt
  -- | Generate Random Value of VP@A as a ValueType of VP@B
  | CRSRandomBy         (VariablePosition vp vi v) (VariablePosition vp vi v)
  -- | Generate Random Value of VP@A as a given ValueType with parameters vpC, vpD, and vpE
  | CRSRandomWith       (VariablePosition vp vi v) vt (VariablePosition vp vi v) (VariablePosition vp vi v) (VariablePosition vp vi v)
  -- | Generate Random Value of VP@A as a ValueType of VP@B with parameters vpC, vpD, and vpE
  | CRSRandomWithBy     (VariablePosition vp vi v) (VariablePosition vp vi v) (VariablePosition vp vi v) (VariablePosition vp vi v) (VariablePosition vp vi v)
  -- | Read & Parse StrValue of VP@A as a script and store the script to VP@B
  | CRSParseScript      (VariablePosition vp vi v) (VariablePosition vp vi v)
  -- | Log a content of VP@B to VP@B
  | CRSLog              (VariablePosition vp vi v) (VariablePosition vp vi v)
  -- | To0
  | CRSTo0 CHeader
  -- | To1 passes one Value of VP@A
  | CRSTo1 CHeader (VariablePosition vp vi v)
  -- | To2 passes one Value of VP@A and VP@B
  | CRSTo2 CHeader (VariablePosition vp vi v) (VariablePosition vp vi v)
  -- | To3 passes one Value of VP@A, VP@B and VP@C
  | CRSTo3 CHeader (VariablePosition vp vi v) (VariablePosition vp vi v) (VariablePosition vp vi v)
  -- | To4 passes one Value of VP@A, VP@B, VP@C and VP@D
  | CRSTo4 CHeader (VariablePosition vp vi v) (VariablePosition vp vi v) (VariablePosition vp vi v) (VariablePosition vp vi v)
  -- | To5 passes one Value of VP@A, VP@B, VP@C, VP@D and VP@E
  | CRSTo5 CHeader (VariablePosition vp vi v) (VariablePosition vp vi v) (VariablePosition vp vi v) (VariablePosition vp vi v) (VariablePosition vp vi v)
  -- | To6 passes one Value of VP@A, VP@B, VP@C and VP@D, VP@E and VP@F
  | CRSTo6 CHeader (VariablePosition vp vi v) (VariablePosition vp vi v) (VariablePosition vp vi v) (VariablePosition vp vi v) (VariablePosition vp vi v) (VariablePosition vp vi v)
  -- | To7 passes one Value of VP@A, VP@B, VP@C and VP@D, VP@E, VP@F and VP@G
  | CRSTo7 CHeader (VariablePosition vp vi v) (VariablePosition vp vi v) (VariablePosition vp vi v) (VariablePosition vp vi v) (VariablePosition vp vi v) (VariablePosition vp vi v) (VariablePosition vp vi v)
  -- | To8 passes one Value of VP@A, VP@B, VP@C and VP@D, VP@E, VP@F, VP@G and VP@H
  | CRSTo8 CHeader (VariablePosition vp vi v) (VariablePosition vp vi v) (VariablePosition vp vi v) (VariablePosition vp vi v) (VariablePosition vp vi v) (VariablePosition vp vi v) (VariablePosition vp vi v) (VariablePosition vp vi v)
  -- | Ext0
  | CRSExt (eis vp vi v vt co)
  deriving (Eq, Ord)
