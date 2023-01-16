{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}

module API (API, GitlabAPI (..)) where

import GHC.Generics
import qualified Meta 
import qualified Project 
import Servant
import Servant.API.Generic

type API = Header "PRIVATE-TOKEN" String :> "api" :> "v4" :> NamedRoutes GitlabAPI

data GitlabAPI mode = GitlabAPI
  { meta :: mode :- Meta.API,
    project :: mode :- Project.API
  }
  deriving stock (Generic)
