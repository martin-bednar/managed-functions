{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Managed.Probe.ToProbe
  ( toProbe
  , ToProbe(..)
  ) where

import Control.Monad.Catch (MonadThrow)
import Data.Managed
import Data.Typeable (Proxy(..), TypeRep, Typeable, typeOf)
import Managed.Exception (badNumberOfArgs, noParseArg, throwM)
import Managed.Probe.Internal.Params (paramsCnt)

-- | Converts any suitable function to a 'Probe'
toProbe ::
     forall e fn. (Typeable fn, ToProbe fn e)
  => fn
  -> Probe e
toProbe x =
  let t = typeOf x
   in Probe {typeRep = t, call = checkArgs t (apply (Proxy @e) x)}

-- | Class of functions that can be converted to a Probe
class ToProbe fn e
  -- | Read arguments from a list, apply them to a function, and encode the result
  where
  apply :: Proxy e -> fn -> [In e] -> IO (Out e)

instance {-# OVERLAPPABLE #-} (Encode a e) => ToProbe a e where
  apply _ c [] = return $ (encode @a @e) c

instance {-# OVERLAPPING #-} (Encode a e) => ToProbe (IO a) e where
  apply _ c [] = (encode @a @e) <$> c

instance {-# OVERLAPPING #-} (Decode a e, ToProbe b e) =>
                             ToProbe (a -> b) e where
  apply _ f (x:xs) = do
    r <- decodeSingle (Proxy @e) x
    apply (Proxy @e) (f r) xs

-- Helper functions
decodeSingle ::
     forall a e m. (MonadThrow m, Decode a e)
  => Proxy e
  -> In e
  -> m a
decodeSingle _ x'
  | (Just x) <- (decode @a @e) x' = return x
  | otherwise = throwM noParseArg

checkArgs :: TypeRep -> ([a] -> IO b) -> [a] -> IO b
checkArgs t = withArgs (paramsCnt t)

withArgs :: Int -> ([a] -> IO b) -> [a] -> IO b
withArgs n f args
  | n == length args = f args
  | otherwise = throwM $ badNumberOfArgs n args
