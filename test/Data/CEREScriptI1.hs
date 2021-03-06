module Data.CEREScriptI1 where


import           Data.Text                      ( Text )


import           Data.ArchCEREScript
import           Data.ArchCEREScript.Type
import           Data.ArchCEREScript.VariablePosition


type CEREScriptE1 = ArchCERES VP1 VI1 V1 VT1 CO1 (EIS VP1 VI1 V1 VT1 CO1)

-- NOTE: Have five extended instructions
data EIS vp vi vc v vt vo
  = EISAbc
  | EISBcd (VariablePosition vp vi vc)
  | EISCde (VariablePosition vp vi vc) (VariablePosition vp vi vc)
  | EISExtended0 CHeader
  | EISExtended1 CHeader (VariablePosition vp vi vc)
  | EISExtended2 CHeader (VariablePosition vp vi vc) (VariablePosition vp vi vc)

-- NOTE: Have only
data VP1 = AtMap | AtTable | AtHere
data VI1 vc = VI1I IIdx | VI1N NIdx | VI1V vc
-- NOTE: Have two custom type
data VT1 = VTInt | VTStr | VTCustom1 | VTCustom2
data V1 = VInt Int | VStr String | V1 CustomData1 | V2 CustomData2

data CO1 = COA OHeader | COR OHeader | COB OHeader | COT OHeader

-- NOTE: Custom type definition
data CustomData1 = Enum1 | Enum2
data CustomData2 = CT2 { ct2i :: Int, ct2s :: String }

-- NOTE: Sample of CEREScriptE1
aCRS1, aCRS2 :: CEREScriptE1
aCRS1 = CRSExt (EISBcd (VP AtHere (VI1V (VInt 1))))
aCRS2 = CRSDeleteVariable (VP AtMap (VI1I 1))
