{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Data.Managed.Agent where

import Data.Managed.Probe

import Data.Map (Map)

type ProbeID = String

type Agent e = Map ProbeID (Probe e)
