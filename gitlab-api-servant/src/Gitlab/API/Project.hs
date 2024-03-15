{-# LANGUAGE DataKinds #-}

module Gitlab.API.Project
  ( API,
    ProjectAPI (..),
    SingleProjectAPI (..),
    JobAPI (..),
    MergeRequestAPI (..),
    RepositoryAPI (..),
  )
where

import Gitlab.Branch qualified
import Gitlab.Job qualified
import Gitlab.Lib (Id)
import Gitlab.MergeRequest qualified
import Gitlab.Project qualified
import Servant
import Servant.API.Generic

type API = "projects" :> NamedRoutes ProjectAPI

data ProjectAPI mode = ProjectAPI
  { getAllProjects :: mode :- Get '[JSON] [Gitlab.Project.Project],
    singleProject :: mode :- Capture "project ID" (Id Gitlab.Project.Project) :> NamedRoutes SingleProjectAPI
  }
  deriving stock (Generic)

data SingleProjectAPI mode = SingleProjectAPI
  { getProject :: mode :- Get '[JSON] Gitlab.Project.Project,
    jobs :: mode :- "jobs" :> NamedRoutes JobAPI,
    mergeRequests :: mode :- "merge_requests" :> NamedRoutes MergeRequestAPI,
    repository :: mode :- "repository" :> NamedRoutes RepositoryAPI
  }
  deriving stock (Generic)

newtype JobAPI mode = JobAPI
  { getSingleJob :: mode :- Capture "job ID" (Id Gitlab.Job.Job) :> Get '[JSON] Gitlab.Job.Job
  }
  deriving stock (Generic)

-- todo: is this a good solution?
data Author

newtype MergeRequestAPI mode = MergeRequestAPI
  { getMergeRequests ::
      mode
        :- QueryParam "state" String -- todo: sum type for state filter
          :> QueryParam "author_id" (Id Author)
          :> Get '[JSON] [Gitlab.MergeRequest.MergeRequest]
  }
  deriving stock (Generic)

newtype RepositoryAPI mode = RepositoryAPI
  { getBranches :: mode :- "branches" :> Get '[JSON] [Gitlab.Branch.Branch]
  }
  deriving stock (Generic)
