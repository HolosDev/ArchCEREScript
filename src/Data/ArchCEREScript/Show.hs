module Data.ArchCEREScript.Show where


import Data.IntMap.Strict as SIM
import Data.Map.Strict as SM

import TextShow ()
import TextShow as TS

import Data.ArchCEREScript.Show.Util


instance TextShow a => TextShow (IntMap a) where
  showb im = wrapSquare mapInternal
   where
    mapInternal :: Builder
    mapInternal = foldr1 (\v b -> v <> comma <> b) builderList
    builderList = Prelude.map renderKV . SIM.toList $ im
    renderKV (k, v) = wrapRound (showb k <> TS.singleton '|' <> showb v)
instance (TextShow idx, TextShow a) => TextShow (Map idx a) where
  showb m = wrapSquare mapInternal
   where
    mapInternal :: Builder
    mapInternal = foldr1 (\v b -> v <> comma <> b) builderList
    builderList = Prelude.map renderKV . SM.toList $ m
    renderKV (k, v) = wrapRound (showb k <> TS.singleton '|' <> showb v)

showbList :: TextShow a => [a] -> Builder
showbList aList = wrapSquare mapInternal
 where
  mapInternal :: Builder
  mapInternal = foldr1 (\v b -> v <> comma <> b) . Prelude.map showb $ aList
