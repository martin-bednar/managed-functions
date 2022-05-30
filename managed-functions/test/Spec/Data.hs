{-# LANGUAGE TypeApplications #-}

module Spec.Data where

import Data.Typeable (Proxy(Proxy), TypeRep, typeRep)
import Managed hiding (typeRep)

data MyData
  = A
  | B
  | C
  deriving (Show, Read, Eq)

nullary :: MyData
nullary = A

probeN :: Probe SR
probeN = toProbe nullary

unary :: MyData -> MyData
unary _ = B

probeU :: Probe SR
probeU = toProbe unary

binary :: MyData -> MyData -> MyData
binary _ _ = C

probeB :: Probe SR
probeB = toProbe binary

unaryIO :: MyData -> IO MyData
unaryIO = return

probeIO :: Probe SR
probeIO = toProbe unaryIO

probeIntCharStr :: Probe SR
probeIntCharStr =
  toProbe (replicate :: Int -> Char -> [Char])

int :: TypeRep
int = typeRep (Proxy @Int)

char :: TypeRep
char = typeRep (Proxy @Char)

intChar :: TypeRep
intChar = typeRep (Proxy @(Int -> Char))

intCharChar :: TypeRep
intCharChar = typeRep (Proxy @(Int -> Char -> Char))
