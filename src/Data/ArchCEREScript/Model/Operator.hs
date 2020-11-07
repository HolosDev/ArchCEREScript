module Data.ArchCEREScript.Model.Operator where


import TextShow
import TextShow.Data.ShortText ()

import Data.ArchCEREScript.Type (Operator)


data CERESOperator
  = COVaChkTo
  | COVaGetPtr
  | COVaSetPtr
  | COVa Operator -- Custom Variable Operator
  | COVlCnvBy
  | COVlRandomBy
  | COVlRandomWith
  | COVlRandomWithBy
  | COVlParse
  | COVl Operator -- Custom Value Operator
  | COAAdd
  | COAMul
  | COASub
  | COADiv
  | COAMod
  | COAEql
  | COACmp
  | COANeg
  | COA Operator -- Custom Arithmetic Operator
  | COBNot
  | CORSwp
  | CORMov
  | COR Operator -- Custom Register Operator
  | COTTake
  | COTDrop
  | COTSplit
  | COTTrim
  | COTAppend
  | COTConcat
  | COTInter
  | COTReplace
  | COTReplaceTo
  | COTReplaceBy
  | COTReplaceByTo
  | COTReverse
  | COTJustify
  | COTLength
  | COTIsNull
  | COTIsPrefix
  | COTIsInfix
  | COTIsSuffix
  | COT Operator -- Custom Text Operator
  | COE1 Operator -- Custom Extendable Operator
  | COE2 Operator -- Custom Extendable Operator
  | COE3 Operator -- Custom Extendable Operator
  | COE4 Operator -- Custom Extendable Operator
  deriving (Eq, Ord)

instance Show CERESOperator where
  show = toString . showb

instance TextShow CERESOperator where
  showb COAAdd = fromText "<Add>"
  showb COASub = fromText "<Sub>"
  showb COAMul = fromText "<Mul>"
  showb COADiv = fromText "<Div>"
  showb COAMod = fromText "<Mod>"
  showb COAEql = fromText "<Eql>"
  showb COACmp = fromText "<Cmp>"
  showb COANeg = fromText "<Neg>"
  showb (COA o) = fromText "<A:" <> showb o <> singleton '>'
  showb COBNot = fromText "<Not>"
  showb CORSwp = fromText "<Swp>"
  showb CORMov = fromText "<Mov>"
  showb (COR o) = fromText "<R:" <> showb o <> singleton '>'
  showb COTTake = fromText "<Take>"
  showb COTDrop = fromText "<Drop>"
  showb COTSplit = fromText "<Split>"
  showb COTTrim = fromText "<Trim>"
  showb COTAppend = fromText "<Append>"
  showb COTConcat = fromText "<Concat>"
  showb COTInter = fromText "<Inter>"
  showb COTReplace = fromText "<Replace>"
  showb COTReverse = fromText "<Reverse>"
  showb COTJustify = fromText "<Justify>"
  showb COTLength = fromText "<Length>"
  showb COTIsNull = fromText "<IsNull>"
  showb COTIsPrefix = fromText "<IsPrefix>"
  showb COTIsInfix = fromText "<IsInfix>"
  showb COTIsSuffix = fromText "<IsSuffix>"
  showb (COT o) = fromText "<T:" <> showb o <> singleton '>'
  showb (COE1 o) = fromText "<E1:" <> showb o <> singleton '>'
  showb (COE2 o) = fromText "<E2:" <> showb o <> singleton '>'
  showb (COE3 o) = fromText "<E3:" <> showb o <> singleton '>'
  showb (COE4 o) = fromText "<E4:" <> showb o <> singleton '>'
