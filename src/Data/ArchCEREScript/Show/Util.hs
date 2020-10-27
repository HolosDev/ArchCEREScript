module Data.ArchCEREScript.Show.Util where


import qualified Data.IntMap.Strict as SIM
import qualified Data.Map.Strict as SM
import Data.Maybe
import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.Trie.Text as Trie

import TextShow
import TextShow.Data.ShortText ()

import Data.ArchCEREScript.Type


showb1 :: TextShow a => Text -> a -> Builder
showb1 n a = fromText n <> singleton '=' <> showb a
showb2 :: (TextShow a, TextShow b) => Text -> a -> b -> Builder
showb2 n a b = fromText n <> singleton '=' <> showb a <> colon <> showb b
showb3 :: (TextShow a, TextShow b, TextShow c) => Text -> a -> b -> c -> Builder
showb3 n a b c = fromText n <> singleton '=' <> showb a <> colon <> showb b <> colon <> showb c
showb4 :: (TextShow a, TextShow b, TextShow c, TextShow d) => Text -> a -> b -> c -> d -> Builder
showb4 n a b c d = fromText n <> singleton '=' <> showb a <> colon <> showb b <> colon <> showb c <> colon <> showb d
showb5 :: (TextShow a, TextShow b, TextShow c, TextShow d, TextShow e) => Text -> a -> b -> c -> d -> e -> Builder
showb5 n a b c d e = fromText n <> singleton '=' <> showb a <> colon <> showb b <> colon <> showb c <> colon <> showb d <> colon <> showb e
showb6 :: (TextShow a, TextShow b, TextShow c, TextShow d, TextShow e, TextShow f) => Text -> a -> b -> c -> d -> e -> f -> Builder
showb6 n a b c d e f = fromText n <> singleton '=' <> showb a <> colon <> showb b <> colon <> showb c <> colon <> showb d <> colon <> showb e <> colon <> showb f
showb7 :: (TextShow a, TextShow b, TextShow c, TextShow d, TextShow e, TextShow f, TextShow g) => Text -> a -> b -> c -> d -> e -> f -> g -> Builder
showb7 n a b c d e f g = fromText n <> singleton '=' <> showb a <> colon <> showb b <> colon <> showb c <> colon <> showb d <> colon <> showb e <> colon <> showb f <> colon <> showb g
showb8 :: (TextShow a, TextShow b, TextShow c, TextShow d, TextShow e, TextShow f, TextShow g, TextShow h) => Text -> a -> b -> c -> d -> e -> f -> g -> h -> Builder
showb8 n a b c d e f g h = fromText n <> singleton '=' <> showb a <> colon <> showb b <> colon <> showb c <> colon <> showb d <> colon <> showb e <> colon <> showb f <> colon <> showb g <> colon <> showb h

showbCS1 :: TextShow a => Text -> a -> Builder
showbCS1 n a = fromText n <> space <> showb a
showbCS2 :: (TextShow a, TextShow b) => Text -> a -> b -> Builder
showbCS2 n a b = fromText n <> space <> showb a <> space <> showb b
showbCS3 :: (TextShow a, TextShow b, TextShow c) => Text -> a -> b -> c -> Builder
showbCS3 n a b c = fromText n <> space <> showb a <> space <> showb b <> space <> showb c
showbCS4 :: (TextShow a, TextShow b, TextShow c, TextShow d) => Text -> a -> b -> c -> d -> Builder
showbCS4 n a b c d = fromText n <> space <> showb a <> space <> showb b <> space <> showb c <> space <> showb d
showbCS5 :: (TextShow a, TextShow b, TextShow c, TextShow d, TextShow e) => Text -> a -> b -> c -> d -> e -> Builder
showbCS5 n a b c d e = fromText n <> space <> showb a <> space <> showb b <> space <> showb c <> space <> showb d <> space <> showb e
showbCS6 :: (TextShow a, TextShow b, TextShow c, TextShow d, TextShow e, TextShow f) => Text -> a -> b -> c -> d -> e -> f -> Builder
showbCS6 n a b c d e f = fromText n <> space <> showb a <> space <> showb b <> space <> showb c <> space <> showb d <> space <> showb e <> space <> showb f
showbCS7 :: (TextShow a, TextShow b, TextShow c, TextShow d, TextShow e, TextShow f, TextShow g) => Text -> a -> b -> c -> d -> e -> f -> g -> Builder
showbCS7 n a b c d e f g = fromText n <> space <> showb a <> space <> showb b <> space <> showb c <> space <> showb d <> space <> showb e <> space <> showb f <> space <> showb g
showbCS8 :: (TextShow a, TextShow b, TextShow c, TextShow d, TextShow e, TextShow f, TextShow g, TextShow h) => Text -> a -> b -> c -> d -> e -> f -> g -> h -> Builder
showbCS8 n a b c d e f g h = fromText n <> space <> showb a <> space <> showb b <> space <> showb c <> space <> showb d <> space <> showb e <> space <> showb f <> space <> showb g <> space <> showb h

showbCSC0 :: IHeader -> CHeader -> Builder
showbCSC0 ih ic = showb ih <> space <> showb ic
showbCSC1 :: TextShow a => IHeader -> CHeader -> a -> Builder
showbCSC1 ih ic a = showb ih <> space <> showb ic <> space <> showb a
showbCSC2 :: (TextShow a, TextShow b) => IHeader -> CHeader -> a -> b -> Builder
showbCSC2 ih ic a b = showb ih <> space <> showb ic <> space <> showb a <> space <> showb b
showbCSC3 :: (TextShow a, TextShow b, TextShow c) => IHeader -> CHeader -> a -> b -> c -> Builder
showbCSC3 ih ic a b c = showb ih <> space <> showb ic <> space <> showb a <> space <> showb b <> space <> showb c
showbCSC4 :: (TextShow a, TextShow b, TextShow c, TextShow d) => IHeader -> CHeader -> a -> b -> c -> d -> Builder
showbCSC4 ih ic a b c d = showb ih <> space <> showb ic <> space <> showb a <> space <> showb b <> space <> showb c <> space <> showb d
showbCSC5 :: (TextShow a, TextShow b, TextShow c, TextShow d, TextShow e) => IHeader -> CHeader -> a -> b -> c -> d -> e -> Builder
showbCSC5 ih ic a b c d e = showb ih <> space <> showb ic <> space <> showb a <> space <> showb b <> space <> showb c <> space <> showb d <> space <> showb e
showbCSC6 :: (TextShow a, TextShow b, TextShow c, TextShow d, TextShow e, TextShow f) => IHeader -> CHeader -> a -> b -> c -> d -> e -> f -> Builder
showbCSC6 ih ic a b c d e f = showb ih <> space <> showb ic <> space <> showb a <> space <> showb b <> space <> showb c <> space <> showb d <> space <> showb e <> space <> showb f
showbCSC7 :: (TextShow a, TextShow b, TextShow c, TextShow d, TextShow e, TextShow f, TextShow g) => IHeader -> CHeader -> a -> b -> c -> d -> e -> f -> g -> Builder
showbCSC7 ih ic a b c d e f g = showb ih <> space <> showb ic <> space <> showb a <> space <> showb b <> space <> showb c <> space <> showb d <> space <> showb e <> space <> showb f <> space <> showb g
showbCSC8 :: (TextShow a, TextShow b, TextShow c, TextShow d, TextShow e, TextShow f, TextShow g, TextShow h) => IHeader -> CHeader -> a -> b -> c -> d -> e -> f -> g -> h -> Builder
showbCSC8 ih ic a b c d e f g h = showb ih <> space <> showb ic <> space <> showb a <> space <> showb b <> space <> showb c <> space <> showb d <> space <> showb e <> space <> showb f <> space <> showb g <> space <> showb h

wrapWith :: Text -> Text -> Builder -> Builder
wrapWith opening closing b = fromText opening <> b <> fromText closing
wrapWith' :: Char -> Char -> Builder -> Builder
wrapWith' opening closing b = singleton opening <> b <> singleton closing

wrapSpace :: Builder -> Builder
wrapSpace b = space <> b <> space
wrapQuot :: Builder -> Builder
wrapQuot b = singleton '\'' <> b <> singleton '\''
wrapDoubleQuot :: Builder -> Builder
wrapDoubleQuot b = singleton '\"' <> b <> singleton '\"'
wrapAngle :: Builder -> Builder
wrapAngle b = singleton '<' <> b <> singleton '>'
wrapRound :: Builder -> Builder
wrapRound b = singleton '(' <> b <> singleton ')'
wrapCurly :: Builder -> Builder
wrapCurly b = singleton '{' <> b <> singleton '}'
wrapSquare :: Builder -> Builder
wrapSquare b = singleton '[' <> b <> singleton ']'
wrapSquareBar :: Builder -> Builder
wrapSquareBar b = fromText "[|" <> b <> fromText "|]"
wrapDelta :: Builder -> Builder
wrapDelta b = fromText "<|" <> b <> fromText "|>"
wrapDoubleSquare :: Builder -> Builder
wrapDoubleSquare b = fromText "[[" <> b <> fromText "]]"
wrapDoubleStick :: Builder -> Builder
wrapDoubleStick b = fromText "||" <> b <> fromText "||"


instance TextShow a => TextShow (SIM.IntMap a) where
  showb im = wrapSquare mapInternal
   where
    mapInternal :: Builder
    mapInternal = foldr1 (\v b -> v <> comma <> b) builderList
    builderList = Prelude.map renderKV . SIM.toList $ im
    renderKV (k, v) = wrapRound (showb k <> singleton '|' <> showb v)
instance (TextShow idx, TextShow a) => TextShow (SM.Map idx a) where
  showb m = wrapSquare mapInternal
   where
    mapInternal :: Builder
    mapInternal = foldr1 (\v b -> v <> comma <> b) builderList
    builderList = Prelude.map renderKV . SM.toList $ m
    renderKV (k, v) = wrapRound (showb k <> singleton '|' <> showb v)

showbListWith :: TextShow a => [a] -> Builder -> Builder
showbListWith aList delimiter = wrapSquare mapInternal
 where
  mapInternal :: Builder
  mapInternal = foldr1 (\v b -> v <> delimiter <> b) . Prelude.map showb $ aList

showbInternalArrayWith :: TextShow a => V.Vector a -> Builder -> Builder
showbInternalArrayWith a delimiter =
  if V.null a
    then blank
    else V.foldr (\v b -> showb v <> delimiter <> b) (showb . V.last $ a) $ V.init a

showbInternalIMapWith :: TextShow a => SIM.IntMap a -> Builder -> Builder -> Builder
showbInternalIMapWith im delimiter inter =
  if SIM.null im
    then blank
    else
      SIM.foldrWithKey
        (\k v b -> showbElem k v <> delimiter <> b)
        (showbElem lk lv)
        $ SIM.deleteMax im
 where
  (lk, lv) = fromJust . SIM.lookupMax $ im
  showbElem k v = showb k <> inter <> showb v

showbInternalMapWith :: (TextShow k, TextShow v) => SM.Map k v -> Builder -> Builder -> Builder
showbInternalMapWith m delimiter inter =
  if SM.null m
    then blank
    else
      SM.foldrWithKey
        (\k v b -> showbElem k v <> delimiter <> b)
        (showbElem lk lv)
        $ SM.deleteMax m
 where
  (lk, lv) = fromJust . SM.lookupMax $ m
  showbElem k v = showb k <> inter <> showb v

showbInternalNMapWith :: TextShow a => Trie.Trie a -> Builder -> Builder -> Builder
showbInternalNMapWith nm delimiter inter =
  if Trie.null nm
    then blank
    else
      Prelude.foldr
        (\(k, v) b -> showbElem k v <> delimiter <> b)
        (showbElem hk hv)
        $ Prelude.tail nmList
 where
  nmList = Trie.toList nm
  (hk, hv) = Prelude.head nmList
  showbElem k v = showb k <> inter <> showb v

blank :: Builder
blank = fromString ""
newline :: Builder
newline = singleton '\n'
tab :: Builder
tab = singleton '\t'
bar :: Builder
bar = singleton '|'
comma :: Builder
comma = singleton ','
space :: Builder
space = singleton ' '
colon :: Builder
colon = singleton ':'
semicolon :: Builder
semicolon = singleton ';'
