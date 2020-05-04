module Data.CEREScriptI1 where


import           Data.Text                      ( Text )


import           Data.ArchCERES.Script
import           Data.ArchCERES.Type
import           Data.ArchCERES.VariablePosition


type CEREScriptE1 = ArchCERES VP1 VI1 V1 VT1 CO1 EIS

-- NOTE: Have five extended instructions
data EIS vp vi v vt vo
  = EISAbc
  | EISBcd (VariablePosition vp vi v)
  | EISCde (VariablePosition vp vi v) (VariablePosition vp vi v)
  | EISExtended0 CHeader
  | EISExtended1 CHeader (VariablePosition vp vi v)
  | EISExtended2 CHeader (VariablePosition vp vi v) (VariablePosition vp vi v)

-- NOTE: Have two custom type
data VT1 = VTSInt | VTSStr | VTSCustom1 | VTSCustom2
data V1 = VTInt Int | VTStr String | VT1 CustomData1 | VT2 CustomData2
-- NOTE: Have only
data VP1 = AtMap | AtTable | AtHere
data VI1 v = VI1I IIdx | VI1N NIdx | VI1V v

data CO1 = COA OHeader | COR OHeader | COB OHeader | COT OHeader

-- NOTE: Custom type definition
data CustomData1 = Enum1 | Enum2
data CustomData2 = CT2 { ct2i :: Int, ct2s :: String }

-- NOTE: Sample of CEREScriptE1
aCRS1, aCRS2 :: CEREScriptE1
aCRS1 = CRSExt (EISBcd (VP AtHere (VI1V (VTInt 1))))
aCRS2 = CRSDeleteVariable (VP AtMap (VI1I 1))
