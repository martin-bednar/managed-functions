module Data.Managed.Probe where

import Data.Data (TypeRep)
import Data.Managed.Encoding

data Probe e =
  Probe
    { call :: [In e] -> IO (Out e)
    , typeRep :: TypeRep
    }

instance Show (Probe e) where
  show p = "<Probe> :: " ++ show (typeRep p)
