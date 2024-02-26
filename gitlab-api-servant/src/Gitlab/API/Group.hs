{-# LANGUAGE DataKinds #-}

module Gitlab.API.Group (API, GroupAPI (..), SingleGroupAPI (..)) where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Validity
import Data.Validity.Path ()
import Gitlab.Lib (Id, Name, Url)
import Path
import Gitlab.Project qualified
import Servant
import Servant.API.Generic
import Gitlab.Group qualified
type API = "groups" :> NamedRoutes GroupAPI

data GroupAPI mode = GroupAPI
  { getAllGroups :: mode :- QueryFlag "all_available" :> Get '[JSON] [Gitlab.Group.Group],
    singleGroup :: mode :- Capture "group ID" (Id Gitlab.Group.Group) :> NamedRoutes SingleGroupAPI
  }
  deriving stock (Generic)

data SingleGroupAPI mode = SingleGroupAPI
  { getGroup :: mode :- Get '[JSON] Gitlab.Group.Group,
    getProjects ::
      mode
        :- "projects"
          :> QueryFlag "include_subgroups"
          :> QueryFlag "with_shared"
          :> QueryFlag "archived"
          :> Get '[JSON] [Gitlab.Project.Project]
  }
  deriving stock (Generic)
