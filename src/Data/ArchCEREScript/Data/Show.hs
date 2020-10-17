module Data.ArchCEREScript.Data.Show where


import Data.IntMap.Strict as SIM
import Data.Map.Strict as SM
import Data.Maybe
import Data.List

import TextShow ()
import TextShow as TS
    ( TextShow(showb), fromLazyText, fromString, singleton, Builder )


instance TextShow a => TextShow (IntMap a) where
  showb im = TS.singleton '[' <> mapInternal <> TS.singleton ']'
   where
    mapInternal :: Builder
    mapInternal =
      if SIM.null im
        then nothing -- how to return nothing
        else SIM.foldrWithKey (\k v b -> b <> TS.singleton ',' <> renderKV k v) base im
     where
      nothing = fromString ""
      (hk, hv) = fromJust . SIM.lookupMin $ im
      renderKV k v = TS.singleton '(' <> showb k <> TS.singleton '|' <> showb v <> TS.singleton ')'
      base = renderKV hk hv
instance (TextShow idx, TextShow a) => TextShow (Map idx a) where
  showb im = TS.singleton '[' <> mapInternal <> TS.singleton ']'
   where
    mapInternal :: Builder
    mapInternal =
      if SM.null im
        then nothing -- how to return nothing
        else SM.foldrWithKey (\k v b -> b <> TS.singleton ',' <> renderKV k v) base im
     where
      nothing = fromString ""
      (hk, hv) = fromJust . SM.lookupMin $ im
      renderKV k v = TS.singleton '(' <> showb k <> TS.singleton '|' <> showb v <> TS.singleton ')'
      base = renderKV hk hv

showbList :: TextShow a => [a] -> Builder
showbList aList = TS.singleton '[' <> mapInternal <> TS.singleton ']'
 where
  mapInternal :: Builder
  mapInternal = foldr1 (<>) . intersperse (fromLazyText "|") . Prelude.map showb $ aList
