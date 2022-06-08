module Managed.Probe.Internal.Params
  ( params
  , paramsCnt
  , returns
  ) where

import Data.Typeable

someFunction :: TypeRep
someFunction = typeRep (Proxy :: Proxy (() -> ()))

isFunction :: TypeRep -> Bool
isFunction x =
  tyConFingerprint (typeRepTyCon x) ==
  tyConFingerprint (typeRepTyCon someFunction)

expand :: TypeRep -> [TypeRep]
expand x =
  if isFunction x
    then flatten (typeRepArgs x)
    else [x]

flatten :: [TypeRep] -> [TypeRep]
flatten = concatMap expand

params :: TypeRep -> [TypeRep]
params = init . expand

paramsCnt :: TypeRep -> Int
paramsCnt = length . params

returns :: TypeRep -> TypeRep
returns = last . expand
