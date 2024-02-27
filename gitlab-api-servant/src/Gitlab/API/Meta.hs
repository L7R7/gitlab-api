{-# LANGUAGE DataKinds #-}

module Gitlab.API.Meta (API, MetaAPI (..)) where

import GHC.Generics
import Gitlab.Meta qualified
import Servant

type API = NamedRoutes MetaAPI

data MetaAPI mode = MetaAPI
  { version :: mode :- "version" :> Get '[JSON] Gitlab.Meta.Version,
    metadata :: mode :- "metadata" :> Get '[JSON] Gitlab.Meta.Metadata
  }
  deriving stock (Generic)
