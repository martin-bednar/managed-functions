{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Managed.Encoding where

import Data.Kind (Type)

class Encoding a where
  type In a :: Type
  type Out a :: Type

class Encode t rep where
  encode :: t -> Out rep

class Decode t rep where
  decode :: In rep -> Maybe t

withEncoding ::
     forall e i o. (Encode o e, Decode i e)
  => (i -> o)
  -> In e
  -> Maybe (Out e)
withEncoding f i = do
  decoded <- decode @i @e i
  let applied = f decoded
  return $ encode @o @e applied
