module Managed.Probe.Internal.Params
  ( params
  , paramsCnt
  , returns
  ) where

import Data.Typeable

expand :: TypeRep -> [TypeRep]
expand x
  | "->" <- tyConName $ typeRepTyCon x = flatten $ typeRepArgs x
  | otherwise = [x]

flatten :: [TypeRep] -> [TypeRep]
flatten = concatMap expand

params :: TypeRep -> [TypeRep]
params = init . expand

paramsCnt :: TypeRep -> Int
paramsCnt = length . params

returns :: TypeRep -> TypeRep
returns = last . expand
