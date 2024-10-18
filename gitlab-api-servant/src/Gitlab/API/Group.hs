{-# LANGUAGE DataKinds #-}

module Gitlab.API.Group (API, GroupAPI (..), SingleGroupAPI (..)) where

import Gitlab.Group qualified
import Gitlab.Lib (Id)
import Gitlab.MergeRequest qualified
import Gitlab.Project qualified
import Servant
import Servant.API.Generic

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
          :> Get '[JSON] [Gitlab.Project.Project],
    getMergeRequests :: mode :- "merge_requests" :> Get '[JSON] [Gitlab.MergeRequest.MergeRequest] -- todo: add support for query parameters
  }
  deriving stock (Generic)
