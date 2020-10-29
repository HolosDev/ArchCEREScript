module Data.ArchCEREScript.Model.VariableIndex where


import TextShow as TS

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
  showb (VII iIdx) = showb1 "VII" iIdx
  showb (VIN nIdx) = showb1 "VIN" nIdx
  showb (VIpN nIdx) = showb1 "VIpN" nIdx
  showb (VIIT iIdx time) = showb2 "VIIT" iIdx time
  showb (VINT nIdx time) = showb2 "VINT" nIdx time
  showb (VIpNT nIdx time) = showb2 "VIpNT" nIdx time
  showb (VIIRI iIdx indices) = showb2 "VIIRI" iIdx indices
  showb (VINRI nIdx indices) = showb2 "VINRI" nIdx indices
  showb (VIpNRI nIdx indices) = showb2 "VIpNRI" nIdx indices
  showb (VIIRIT iIdx indices time) = showb3 "VIIRIT" iIdx indices time
  showb (VINRIT nIdx indices time) = showb3 "VINRIT" nIdx indices time
  showb (VIpNRIT nIdx indices time) = showb3 "VIpNRIT" nIdx indices time
  showb (VIV value) = showb1 "VIV" value
  showb VIAtom = fromText "VIAtom"
  showb VINull = fromText "VINull"
  showb (VIPtr vp) = showb1 "VIPtr" vp
  showb (PVII iIdx) = showb1 "PVII" iIdx
  showb (PVIN nIdx) = showb1 "PVIN" nIdx
  showb (PVIpN nIdx) = showb1 "PVIpN" nIdx
  showb (PVIT time) = showb1 "PVIT" time
  showb (PVIIRI iIdx indices) = showb2 "PVIIRI" iIdx indices
  showb (PVINRI nIdx indices) = showb2 "PVINRI" nIdx indices
  showb (PVIpNRI nIdx indices) = showb2 "PVIpNRI" nIdx indices
  showb (PVIIRIT iIdx indices time) = showb3 "PVIIRIT" iIdx indices time
  showb (PVINRIT nIdx indices time) = showb3 "PVINRIT" nIdx indices time
  showb (PVIpNRIT nIdx indices time) = showb3 "PVIpNRIT" nIdx indices time


data InnerIndex = IIIdx IIdx | INIdx NIdx deriving (Eq, Ord)

instance Show InnerIndex where
  show = toString . showb

instance TextShow InnerIndex where
  showb (IIIdx iIdx) = fromText "II" <> wrapSquareBar (showb iIdx)
  showb (INIdx nIdx) = fromText "NI" <> wrapSquareBar (showb nIdx)
