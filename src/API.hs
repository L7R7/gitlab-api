{-# LANGUAGE DataKinds #-}

module API (API, GitlabAPI (..)) where

import GHC.Generics
import Meta qualified
import Project qualified
import Servant

type API = Header "PRIVATE-TOKEN" String :> "api" :> "v4" :> NamedRoutes GitlabAPI

data GitlabAPI mode = GitlabAPI
  { meta :: mode :- Meta.API,
    project :: mode :- Project.API
  }
  deriving stock (Generic)
