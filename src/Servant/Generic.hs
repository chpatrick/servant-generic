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
  
module Servant.Generic
  ( (:-)
  , AsServerT
  , AsServer
  , AsApi
  , ToServant
  , toServant
  , GenericProduct
  -- * Internals
  , GProduct(..)
  , Generic(..)
  ) where

import          GHC.Generics
import          Servant

-- | A class of generic product types.
class GProduct f where
  type GToServant f
  gtoServant :: f p -> GToServant f

instance GProduct f => GProduct (M1 i c f) where
  type GToServant (M1 i c f) = GToServant f
  gtoServant (M1 x) = gtoServant x

instance (GProduct l, GProduct r) => GProduct (l :*: r) where
  type GToServant (l :*: r) = GToServant l :<|> GToServant r
  gtoServant (l :*: r) = gtoServant l :<|> gtoServant r

instance GProduct (K1 i c) where
  type GToServant (K1 i c) = c
  gtoServant (K1 x) = x

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

-- | A type family that applies an appropriate type family to the @api@ parameter.
-- For example, passing `AsApi` will leave @api@ untouched, while @`AsServerT` m@ will produce @`ServerT` api m@. 
type family mode :- api
infixl 8 :-

-- | A type that specifies that an API record contains an API definition. Only useful at type-level.
data AsApi
type instance AsApi :- api = api

-- | A type that specifies that an API record contains a server implementation.
data AsServerT (m :: * -> *)
type instance AsServerT m :- api = ServerT api m
type AsServer = AsServerT Handler