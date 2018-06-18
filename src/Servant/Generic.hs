{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Servant.Generic
  ( (:-)
  , AsServerT
  , AsServer
  , AsApi
  , AsLink
  , ToServant
  , toServant
  , fromServant
  , GenericProduct
  -- * Internals
  , GProduct(..)
  , Generic(..)

  , fieldLink
  ) where

import          GHC.Generics
import          Servant

-- | A class of generic product types.
class GProduct f where
  type GToServant f
  gtoServant :: f p -> GToServant f
  gfromServant :: GToServant f -> f p

instance GProduct f => GProduct (M1 i c f) where
  type GToServant (M1 i c f) = GToServant f
  gtoServant (M1 x) = gtoServant x
  gfromServant x = M1 (gfromServant x)

instance (GProduct l, GProduct r) => GProduct (l :*: r) where
  type GToServant (l :*: r) = GToServant l :<|> GToServant r
  gtoServant (l :*: r) = gtoServant l :<|> gtoServant r
  gfromServant (l :<|> r) = gfromServant l :*: gfromServant r

instance GProduct (K1 i c) where
  type GToServant (K1 i c) = c
  gtoServant (K1 x) = x
  gfromServant x = K1 x

type GenericProduct a = (Generic a, GProduct (Rep a))

-- | Turns a generic product type into a linear tree of `:<|>` combinators.
-- For example, given
--
-- @
--   data Foo route = Foo
--     { foo :: route :-
--         Get '[PlainText] Text
--     , bar :: route :-
--         Get '[PlainText] Text
--     }
-- @
--
-- @ ToServant (Foo AsApi) ~ Get '[PlainText] Text :\<|\> Get '[PlainText] Text @
type ToServant a = GToServant (Rep a)

-- | See `ToServant`, but at value-level.
toServant :: GenericProduct a => a -> ToServant a
toServant = gtoServant . from

-- | Inverse of `toServant`.
--
-- This can be used to turn 'generated' values such as client functions into records.
--
-- You may need to provide a type signature for the /output/ type (your record type).
fromServant :: GenericProduct a => ToServant a -> a
fromServant = to . gfromServant

-- | A type family that applies an appropriate type family to the @api@ parameter.
-- For example, passing `AsApi` will leave @api@ untouched, while @`AsServerT` m@ will produce @`ServerT` api m@.
type family mode :- api
infixl 3 :-

-- | A type that specifies that an API record contains an API definition. Only useful at type-level.
data AsApi
type instance AsApi :- api = api

-- | A type that specifies that an API record contains a set of links.
--
-- (Useful since servant 0.12)
data AsLink
#if MIN_VERSION_servant(0,14,0)
type instance AsLink :- api = MkLink api Link
#else
type instance AsLink :- api = MkLink api
#endif


-- | A type that specifies that an API record contains a server implementation.
data AsServerT (m :: * -> *)
type instance AsServerT m :- api = ServerT api m
type AsServer = AsServerT Handler

-- | Given an API record field, create a link for that route. Only the field's type is used.
fieldLink :: forall routes endpoint. (IsElem endpoint (ToServant (routes AsApi)), HasLink endpoint)
          => (routes AsApi -> endpoint)
#if MIN_VERSION_servant(0,14,0)
          -> MkLink endpoint Link
#else
          -> MkLink endpoint
#endif
fieldLink _ = safeLink (Proxy :: Proxy (ToServant (routes AsApi))) (Proxy :: Proxy endpoint)
