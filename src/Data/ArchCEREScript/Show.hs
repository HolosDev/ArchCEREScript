module Data.ArchCEREScript.Show where


import TextShow ()
import TextShow as TS


import Data.ArchCEREScript
import Data.ArchCEREScript.Show.Util as ACS


instance (TextShow eis, TextShow (vP eis vi vc v vp vt co), TextShow (vi eis vc v vp vt co), TextShow (vc eis v vp co), TextShow (v eis vp co), TextShow vp, TextShow vt, TextShow co) => Show (ArchCEREScript eis vP vi vc v vp vt co) where
  show = toString . showb

instance (TextShow eis, TextShow (vP eis vi vc v vp vt co), TextShow (vi eis vc v vp vt co), TextShow (vc eis v vp co), TextShow (v eis vp co), TextShow vp, TextShow vt, TextShow co) => TextShow (ArchCEREScript eis vP vi vc v vp vt co) where
  showb (SSeq aInst cNext) = fromText "SSeq<" <> showb aInst <> fromText ">\n" <> showb cNext
  showb (SSeqs instList cNext) = fromText "SSeqs<" <> showb instList <> fromText ">\n" <> showb cNext
  showb (SLoop loopCondition loopScript loopLabel cNext) = fromText "SLoop<" <> showb loopCondition <> comma <> showb loopScript <> comma <> showb loopLabel <> fromText ">\n" <> showb cNext
  showb (SCase branchCondition branchScripts cNext otherwiseScript) =
    fromText "SCase<" <> showb branchCondition <> comma <> showb branchScripts <> comma <> showb otherwiseScript <> fromText ">\n" <> showb cNext
  showb (SPar scripts cNext) = fromText "SPar<" <> showbListWith scripts comma <> fromText ">\n" <> showb cNext
  showb SEnd = fromText "SEnd."

instance (TextShow eis, TextShow (vP eis vi vc v vp vt co), TextShow (vi eis vc v vp vt co), TextShow (vc eis v vp co), TextShow (v eis vp co), TextShow vp, TextShow vt, TextShow co) => Show (ArchCERES eis vP vi vc v vp vt co) where
  show = toString . showb

instance (TextShow eis, TextShow (vP eis vi vc v vp vt co), TextShow (vi eis vc v vp vt co), TextShow (vc eis v vp co), TextShow (v eis vp co), TextShow vp, TextShow vt, TextShow co) => TextShow (ArchCERES eis vP vi vc v vp vt co) where
  showb CRSNoop = fromText "Noop"
  showb CRSBreak{..} =
    fromText "Break" <> maybe blank (\l -> singleton '@' <> wrapSquareBar (fromText l)) breakLabel
  showb CRSClearVariables{..} =
    showbCS1 "ClearVariables" variablePlace
  showb CRSInitVariable{..} =
    showbCS1 "InitVariable" wVP1
  showb CRSInitVariableAt{..} =
    showbCS2 "InitVariableAt" variablePlace wVP1
  showb CRSInitVariableBy{..} =
    showbCS2 "InitVariableBy" wVP1 rVP1
  showb CRSCheckVariable{..} =
    showbCS1 "CheckVariable" rVP1
  showb CRSDeleteVariable{..} =
    showbCS1 "DeleteVariable" wVP1
  showb CRSDo{..} =
    showbCS1 "Do" operator
  showb CRSModifyValue{..} =
    showbCS2 "ModifyValue" operator wVP1
  showb CRSModifyValue1{..} =
    showbCS3 "ModifyValue1" operator wVP1 rVP1
  showb CRSModifyValue2{..} =
    showbCS4 "ModifyValue2" operator wVP1 rVP1 rVP2
  showb CRSModifyValue3{..} =
    showbCS5 "ModifyValue3" operator wVP1 rVP1 rVP2 rVP3
  showb CRSModifyValue4{..} =
    showbCS6 "ModifyValue4" operator wVP1 rVP1 rVP2 rVP3 rVP4
  showb CRSModifyBothValue{..} =
    showbCS3 "ModifyBothValue" operator wVP1 wVP2
  showb CRSModifyBothValue1{..} =
    showbCS4 "ModifyBothValue1" operator wVP1 wVP2 rVP1
  showb CRSModifyBothValue2{..} =
    showbCS5 "ModifyBothValue2" operator wVP1 wVP2 rVP1 rVP2
  showb CRSModifyBothValue3{..} =
    showbCS6 "ModifyBothValue3" operator wVP1 wVP2 rVP1 rVP2 rVP3
  showb CRSModifyBothValue4{..} =
    showbCS7 "ModifyBothValue4" operator wVP1 wVP2 rVP1 rVP2 rVP3 rVP4
  showb CRSCopyValue{..} =
    showbCS2 "CopyValue" rVP1 wVP1
  showb CRSConvertValue{..} =
    showbCS2 "ConvertValue" wVP1 valueType
  showb CRSRandom{..} =
    showbCS2 "Random" wVP1 valueType
  showb CRSLog{..} =
    showbCS2 "Log" rVP1 wVP1
  showb CRSExt{..} = showb eis
