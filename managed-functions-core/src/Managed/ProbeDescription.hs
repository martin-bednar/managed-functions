module Managed.ProbeDescription
  ( mkDescription
  , human
  ) where

import Data.Managed
import Managed.Probe

mkDescription :: ProbeID -> Probe e -> ProbeDescription
mkDescription pid probe =
  ProbeDescription
    { probeID = pid
    , probeType = show $ typeRep probe
    , probeParams = Prelude.map show $ params probe
    , probeReturns = show $ returns probe
    }

-- | Create a human-readable description from a 'ProbeDescription'
human :: ProbeDescription -> String
human pd = concat [probeID pd, " :: ", probeType pd]
