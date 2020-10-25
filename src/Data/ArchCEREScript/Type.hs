module Data.ArchCEREScript.Type where

import Data.IntMap (IntMap)
import Data.IntMap.Strict as SIM
import Data.Map (Map)
import Data.Map.Strict as SM
import Data.Text (Text)
import Data.Text.Short (ShortText)
import Data.Trie.Text (Trie)
import Data.Vector (Vector)

import TextShow
import TextShow.Data.Vector

type Operator = ShortText
type Category = ShortText

-- TODO: Check there is obvious difference on performance between ByteString and Lazy Text
type IHeader = ShortText
type CHeader = ShortText
type OHeader = ShortText

type Branch = Text
type Priority = Int

type Message = Text

type ID = Int
type Str = Text

type IIdx = Int
type NIdx = Text

type Name = Text
type Time = Int

type Array a = Vector a
type IMap a = IntMap a
type NMap a = Trie a

type SMap a = SIM.IntMap a
type VMap v a = SM.Map v a

data Atom = Atom deriving (Eq, Ord, Enum)
