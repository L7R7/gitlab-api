{-# LANGUAGE DataKinds #-}

module Gitlab.API.Meta (API, MetaAPI (..)) where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Text
import Data.Validity
import Data.Validity.Text ()
import GHC.Generics
import Servant
import Gitlab.Meta qualified
type API = NamedRoutes MetaAPI

data MetaAPI mode = MetaAPI
  { version :: mode :- "version" :> Get '[JSON] Gitlab.Meta.Version,
    metadata :: mode :- "metadata" :> Get '[JSON] Gitlab.Meta.Metadata
  }
  deriving stock (Generic)
