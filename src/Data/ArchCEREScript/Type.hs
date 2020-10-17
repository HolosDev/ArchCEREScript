module Data.ArchCEREScript.Type where

import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as SIM (
  IntMap,
 )
import Data.Text (Text)
import Data.Text.Short (ShortText)
import Data.Trie.Text (Trie)
import Data.Vector (Vector)

import TextShow
import TextShow.Data.Vector

type Operator = Text
type Category = Text

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

data Atom = Atom deriving (Eq, Ord, Enum, Read)

instance Show Atom where
  show Atom = "Atom"

instance TextShow Atom where
  showb Atom = fromLazyText "Atom"
