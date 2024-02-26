{-# LANGUAGE DataKinds #-}

module Gitlab.API.Project ( API, ProjectAPI (..), SingleProjectAPI (..), JobAPI (..)) where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Validity
import Data.Validity.Path ()
import Gitlab.Job qualified
import Gitlab.Lib (Id, Name, Ref, Url)
import Path
import Servant
import Gitlab.Project qualified
import Servant.API.Generic

type API = "projects" :> NamedRoutes ProjectAPI

data ProjectAPI mode = ProjectAPI
  { getAllProjects :: mode :- Get '[JSON] [Gitlab.Project.Project],
    singleProject :: mode :- Capture "project ID" (Id Gitlab.Project.Project) :> NamedRoutes SingleProjectAPI
  }
  deriving stock (Generic)

data SingleProjectAPI mode = SingleProjectAPI
  { getProject :: mode :- Get '[JSON] Gitlab.Project.Project,
    jobs :: mode :- "jobs" :> NamedRoutes JobAPI
  }
  deriving stock (Generic)

data JobAPI mode = JobAPI
  { getSingleJob :: mode :- Capture "job ID" (Id Gitlab.Job.Job) :> Get '[JSON] Gitlab.Job.Job
  }
  deriving stock (Generic)
