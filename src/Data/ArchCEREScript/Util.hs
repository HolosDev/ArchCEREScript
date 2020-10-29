module Data.ArchCEREScript.Util where

import Data.ArchCEREScript

{- ArchCERES Class

The class used for get Read/Write key efficiently by faster pattern matching

-}

data ACSClass
  -- | Which takes no target VariablePosition
  = ACSC0
  -- | Which takes one target VariablePosition
  | ACSC1
  -- | Which takes two target VariablePositions
  | ACSC2
  -- | Which takes three target VariablePositions
  | ACSC3
  -- | Which takes four target VariablePositions
  | ACSC4
  -- | Which takes unknown number of target VariablePositions
  | ACSCU
  -- | Unrecognized ACS
  | ACSCN
  deriving (Eq, Ord, Enum)

classOf :: ArchCERES eis vP vi vc v vp vt co -> ACSClass
classOf acs = case acs of
  CRSNoop -> ACSC0
-- ...
  _ -> ACSCN
