module Data.ArchCEREScript.Model.VariableIndex where


import TextShow as TS

import Data.ArchCEREScript.Model.VariablePlace
import Data.ArchCEREScript.Show.Util
import Data.ArchCEREScript.Type
import Data.ArchCEREScript.Model.VariablePosition


-------------------------------- # VariableIndex # --------------------------------

-- *pN: Partial NIdx: VIN "ABC" => VIpN "AB"
-- PVI*: Partial Index: VIIN 12 "ABC" => PVI 12

data VariableIndex eis vc v vp vt co
  = VII IIdx
  | VIN NIdx
  | VIpN NIdx
  | VIIT IIdx Time
  | VINT NIdx Time
  | VIpNT NIdx Time
  | VIIRI IIdx [IIdx]
  | VINRI NIdx [IIdx]
  | VIpNRI NIdx [IIdx]
  | VIIRIT IIdx [IIdx] Time
  | VINRIT NIdx [IIdx] Time
  | VIpNRIT NIdx [IIdx] Time
  | VIV (vc eis v vp co)
  | VIAtom
  | VINull
  | VIPtr (VariablePosition eis VariableIndex vc v vp vt co)
  | PVII IIdx
  | PVIN NIdx
  | PVIpN NIdx
  | PVIT Time
  | PVIIRI IIdx [IIdx]
  | PVINRI NIdx [IIdx]
  | PVIpNRI NIdx [IIdx]
  | PVIIRIT IIdx [IIdx] Time
  | PVINRIT NIdx [IIdx] Time
  | PVIpNRIT NIdx [IIdx] Time
  | VIComplex (VariableIndex eis vc v vp vt co) [InnerIndex]
  deriving (Eq, Ord)

instance (TextShow eis, TextShow (vc eis v vp co), TextShow (v eis vp co), TextShow vp, TextShow vt, TextShow co) => Show (VariableIndex eis vc v vp vt co) where
  show = toString . showb

instance (TextShow eis, TextShow (vc eis v vp co), TextShow (v eis vp co), TextShow vp, TextShow vt, TextShow co) => TextShow (VariableIndex eis vc v vp vt co) where
  showb (VII idx) = showb1 "VII" idx
  showb (VIN nKey) = showb1 "VIN" nKey
  showb (VIpN nKey) = showb1 "VIpN" nKey
  showb (VIIT idx time) = showb2 "VIIT" idx time
  showb (VINT nKey time) = showb2 "VINT" nKey time
  showb (VIpNT nKey time) = showb2 "VIpNT" nKey time
  showb (VIIRI idx indices) = showb2 "VIIRI" idx indices
  showb (VINRI nKey indices) = showb2 "VINRI" nKey indices
  showb (VIpNRI nKey indices) = showb2 "VIpNRI" nKey indices
  showb (VIIRIT idx indices time) = showb3 "VIIRIT" idx indices time
  showb (VINRIT nKey indices time) = showb3 "VINRIT" nKey indices time
  showb (VIpNRIT nKey indices time) = showb3 "VIpNRIT" nKey indices time
  showb (VIV value) = showb1 "VIV" value
  showb VIAtom = fromText "VIAtom"
  showb VINull = fromText "VINull"
  showb (VIPtr vp) = showb1 "VIPtr" vp
  showb (PVII idx) = showb1 "PVII" idx
  showb (PVIN nKey) = showb1 "PVIN" nKey
  showb (PVIpN nKey) = showb1 "PVIpN" nKey
  showb (PVIT time) = showb1 "PVIT" time
  showb (PVIIRI idx indices) = showb2 "PVIIRI" idx indices
  showb (PVINRI nKey indices) = showb2 "PVINRI" nKey indices
  showb (PVIpNRI nKey indices) = showb2 "PVIpNRI" nKey indices
  showb (PVIIRIT idx indices time) = showb3 "PVIIRIT" idx indices time
  showb (PVINRIT nKey indices time) = showb3 "PVINRIT" nKey indices time
  showb (PVIpNRIT nKey indices time) = showb3 "PVIpNRIT" nKey indices time


data InnerIndex = IIIdx IIdx | INIdx NIdx deriving (Eq, Ord)

instance Show InnerIndex where
  show = toString . showb

instance TextShow InnerIndex where
  showb (IIIdx iIdx) = fromText "II" <> showb iIdx
  showb (INIdx nIdx) = fromText "NI" <> showb nIdx
