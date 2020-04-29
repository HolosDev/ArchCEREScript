module Data.ArchCERES.Type where


import           Data.ByteString                ( ByteString )
import qualified Data.Text                     as T
import           Data.Text.Lazy                 ( Text )
import           Data.IntMap                    ( IntMap )
import           Data.Trie.Text                 ( Trie )
import           Data.Vector                    ( Vector )

import           TextShow
import           TextShow.Data.Vector


type Operator = T.Text
type Category = T.Text
-- TODO: Check there is obvious difference on performance between ByteString and Lazy Text
type IHeader = ByteString
type CHeader = ByteString

type Branch = T.Text
type Priority = Int

type Message = Text

type ID = Int
type Str = Text

type IIdx = Int
type NIdx = T.Text

type Name = Text
type Time = Int

type Array a = Vector a
type IMap a = IntMap a
type NMap a = Trie a


data Atom = Atom deriving (Eq,Ord,Enum,Read)

instance Show Atom where
  show Atom = "Atom"

instance TextShow Atom where
  showb Atom = fromLazyText "Atom"
