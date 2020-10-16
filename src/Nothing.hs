module Nothing where


import TextShow


data X a = X a

instance TextShow a => Show (X a) where
  show = toString . showb

instance TextShow a => TextShow (X a) where
  showb (X a) = showb a
