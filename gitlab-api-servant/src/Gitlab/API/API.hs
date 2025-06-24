{-# LANGUAGE DataKinds #-}

module Gitlab.API.API (API, GitlabAPI (..)) where

import GHC.Generics
import Gitlab.API.Group qualified
import Gitlab.API.Meta qualified
import Gitlab.API.Project qualified
import Servant

type API = Header "PRIVATE-TOKEN" String :> Header "User-Agent" String :> "api" :> "v4" :> NamedRoutes GitlabAPI

data GitlabAPI mode = GitlabAPI
  { meta :: mode :- Gitlab.API.Meta.API,
    project :: mode :- Gitlab.API.Project.API,
    group :: mode :- Gitlab.API.Group.API
  }
  deriving stock (Generic)
