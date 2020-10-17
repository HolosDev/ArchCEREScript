module Data.ArchCEREScript.Data.Show where


import Data.IntMap.Strict as IM
import Data.Maybe

import TextShow ()
import TextShow as TS


instance TextShow a => TextShow (IntMap a) where
  showb im = TS.singleton '[' <> mapInternal <> TS.singleton ']'
   where
    mapInternal :: Builder
    mapInternal =
      if IM.null im
        then nothing -- how to return nothing
        else foldrWithKey (\k v b -> b <> TS.singleton ',' <> renderKV k v) base im
     where
      nothing = fromString ""
      (hk, hv) = fromJust . lookupMin $ im
      renderKV k v = TS.singleton '(' <> showb k <> TS.singleton '|' <> showb v <> TS.singleton ')'
      base = renderKV hk hv
