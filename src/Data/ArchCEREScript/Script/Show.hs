module Data.ArchCEREScript.Script.Show where


import TextShow ()
import TextShow as TS


import Data.ArchCEREScript.Show as ACS
import Data.ArchCEREScript.Script
import Data.ArchCEREScript.Show.Util
import Data.ArchCEREScript.VariablePosition.Show ()


instance (TextShow vp, TextShow vi, TextShow vc, TextShow vt, TextShow co, TextShow eis) => Show (ArchCEREScript vp vi vc vt co eis) where
  show = toString . showb

instance (TextShow vp, TextShow vi, TextShow vc, TextShow vt, TextShow co, TextShow eis) => TextShow (ArchCEREScript vp vi vc vt co eis) where
  showb (SSeq aInst cNext) = fromLazyText "SSeq<" <> showb aInst <> fromLazyText ">\n" <> showb cNext
  showb (SSeqs instList cNext) = fromLazyText "SSeqs<" <> showb instList <> fromLazyText ">\n" <> showb cNext
  showb (SLoop loopCondition loopScript cNext) = fromLazyText "SLoop<" <> showb loopCondition <> comma <> showb loopScript <> fromLazyText ">\n" <> showb cNext
  showb (SCase branchCondition branchScripts cNext otherwiseScript) =
    fromLazyText "SCase<" <> showb branchCondition <> comma <> showb branchScripts <> comma <> showb otherwiseScript <> fromLazyText ">\n" <> showb cNext
  showb (SPar scripts cNext) = fromLazyText "SPar<" <> ACS.showbList scripts <> fromLazyText ">\n" <> showb cNext
  showb SEnd = fromLazyText "SEnd."

instance (TextShow vp, TextShow vi, TextShow vt, TextShow co, TextShow eis) => Show (ArchCERES vp vi vc vt co eis) where
  show = toString . showb

instance (TextShow vp, TextShow vi, TextShow vt, TextShow co, TextShow eis) => TextShow (ArchCERES vp vi vc vt co eis) where
  showb CRSNoop = fromLazyText "Noop"
  showb (CRSInitVariable vpA vpB) =
    showbCS2 "InitVariable" vpA vpB
  showb (CRSCheckVariable vpA vpB) =
    showbCS2 "CheckVariable" vpA vpB
  showb (CRSDeleteVariable vp) =
    showbCS1 "DeleteVariable" vp
  showb (CRSModifyValue1 vpA cOper) =
    showbCS2 "ModifyValue1" vpA cOper
  showb (CRSModifyValue2 vpA vpB cOper) =
    showbCS3 "ModifyValue2" vpA vpB cOper
  showb (CRSModifyValue3 vpA vpB vpC cOper) =
    showbCS4 "ModifyValue3" vpA vpB vpC cOper
  showb (CRSModifyValue4 vpA vpB vpC vpD cOper) =
    showbCS5 "ModifyValue4" vpA vpB vpC vpD cOper
  showb (CRSCopyValue vpA vpB) =
    showbCS2 "CopyValue" vpA vpB
  showb (CRSConvertValue vp vType) =
    showbCS2 "ConvertValue" vp vType
  showb (CRSConvertValueBy vpA vpB) =
    showbCS2 "ConvertValueBy" vpA vpB
  showb (CRSConvertValueWith vpA vpB) =
    showbCS2 "ConvertValueWith" vpA vpB
  showb (CRSReplaceText vp) =
    showbCS1 "ReplaceText" vp
  showb (CRSReplaceTextTo vpA vpB) =
    showbCS2 "ReplaceTextTo" vpA vpB
  showb (CRSReplaceTextBy vpA vpB vpC) =
    showbCS3 "ReplaceTextBy" vpA vpB vpC
  showb (CRSReplaceTextByTo vpA vpB vpC vpD) =
    showbCS4 "ReplaceTextByTo" vpA vpB vpC vpD
  showb (CRSGetPointer vpA vpB) =
    showbCS2 "GetPointer" vpA vpB
  showb (CRSSetPointer vpA vpB) =
    showbCS2 "SetPointer" vpA vpB
  showb (CRSRandom vp vType) =
    showbCS2 "Random" vp vType
  showb (CRSRandomBy vpA vpB) =
    showbCS2 "RandomBy" vpA vpB
  showb (CRSRandomWith vpA vtB vpC vpD vpE) =
    showbCS5 "RandomWith" vpA vtB vpC vpD vpE
  showb (CRSRandomWithBy vpA vpB vpC vpD vpE) =
    showbCS5 "RandomWithBy" vpA vpB vpC vpD vpE
  showb (CRSParseScript vpA vpB) =
    showbCS2 "ParseScript" vpA vpB
  showb (CRSLog vpA vpB) =
    showbCS2 "Log" vpA vpB
  showb (CRSTo0 cH) =
    showbCSC0 "To0" cH
  showb (CRSTo1 cH vpA) =
    showbCSC1 "To1" cH vpA
  showb (CRSTo2 cH vpA vpB) =
    showbCSC2 "To2" cH vpA vpB
  showb (CRSTo3 cH vpA vpB vpC) =
    showbCSC3 "To3" cH vpA vpB vpC
  showb (CRSTo4 cH vpA vpB vpC vpD) =
    showbCSC4 "To4" cH vpA vpB vpC vpD
  showb (CRSTo5 cH vpA vpB vpC vpD vpE) =
    showbCSC5 "To5" cH vpA vpB vpC vpD vpE
  showb (CRSTo6 cH vpA vpB vpC vpD vpE vpF) =
    showbCSC6 "To6" cH vpA vpB vpC vpD vpE vpF
  showb (CRSTo7 cH vpA vpB vpC vpD vpE vpF vpG) =
    showbCSC7 "To7" cH vpA vpB vpC vpD vpE vpF vpG
  showb (CRSTo8 cH vpA vpB vpC vpD vpE vpF vpG vpH) =
    showbCSC8 "To8" cH vpA vpB vpC vpD vpE vpF vpG vpH
  showb (CRSToList cH vpList) =
    fromLazyText "ToList" <> showb cH <> TS.showbList vpList
  showb (CRSExt ei) = showb ei
