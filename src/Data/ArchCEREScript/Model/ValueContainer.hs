module Data.ArchCEREScript.Model.ValueContainer where


import TextShow as TS

import Data.ArchCEREScript.Type
import Data.ArchCEREScript.Show.Util


-------------------------------- ValueContainer --------------------------------

data ValueContainer eis v vp co = VC
  { value :: (v eis vp co)
  , valueInfo :: ValueInfo
  }
  deriving (Eq, Ord)

instance (TextShow eis, TextShow (v eis vp co), TextShow vp, TextShow co) => Show (ValueContainer eis v vp co) where
  show = toString . showb

instance (TextShow eis, TextShow (v eis vp co), TextShow vp, TextShow co) => TextShow (ValueContainer eis v vp co) where
  -- FIXME: Implement it better
  showb (VC value valueInfo) = fromText "VC" <> wrapDoubleStick (showb value <> colon <> showb valueInfo)

-- TODO: Not yet Implemented
data ValueInfo = ValueInfo
  { valueEdited :: Bool
  , valueInheritance :: ValueInheritanceFlag
  , valueDependencies :: [Branch]
  }
  deriving (Eq, Ord)

instance Show ValueInfo where
  show = toString . showb

instance TextShow ValueInfo where
  -- FIXME: TODO: Implement it
  showb _ = fromText "ValInfo"

data ValueInheritanceFlag = VIFInherit | VIFOnce deriving (Eq, Ord, Enum, Bounded)

instance Show ValueInheritanceFlag where
  show = toString . showb

instance TextShow ValueInheritanceFlag where
  showb VIFInherit = fromText "Inherit"
  showb VIFOnce = fromText "Once"


-------------------------------- Helper for Value --------------------------------

data ValueTyper vt = ValueTyper
  { valueTyperName :: Name
  , valueType :: vt
  }
  deriving (Eq, Ord)
