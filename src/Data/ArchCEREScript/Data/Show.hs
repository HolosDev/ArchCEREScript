module Data.ArchCEREScript.Data.Show where


import Data.IntMap.Strict as SIM
import Data.Map.Strict as SM
import Data.Maybe
import Data.List

import TextShow ()
import TextShow as TS


instance TextShow a => TextShow (IntMap a) where
  showb im = TS.singleton '[' <> mapInternal <> TS.singleton ']'
   where
    mapInternal :: Builder
    mapInternal = foldr1 (<>) . intersperse (fromLazyText ",") $ builderList
    builderList = Prelude.map renderKV . SIM.toList $ im
    renderKV (k, v) = TS.singleton '(' <> showb k <> TS.singleton '|' <> showb v <> TS.singleton ')'
instance (TextShow idx, TextShow a) => TextShow (Map idx a) where
  showb m = TS.singleton '[' <> mapInternal <> TS.singleton ']'
   where
    mapInternal :: Builder
    mapInternal = foldr1 (<>) . intersperse (fromLazyText ",") $ builderList
    builderList = Prelude.map renderKV . SM.toList $ m
    renderKV (k, v) = TS.singleton '(' <> showb k <> TS.singleton '|' <> showb v <> TS.singleton ')'

showbList :: TextShow a => [a] -> Builder
showbList aList = TS.singleton '[' <> mapInternal <> TS.singleton ']'
 where
  mapInternal :: Builder
  mapInternal = foldr1 (<>) . intersperse (fromLazyText "|") . Prelude.map showb $ aList
