{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Project (Project (..), API, ProjectAPI (..), SingleProjectAPI (..), JobAPI (..)) where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Validity
import Data.Validity.Path ()
import qualified Job 
import Lib (Id, Name, Ref, Url)
import Path
import Servant
import Servant.API.Generic

data Project = Project
  { projectId :: Id Project,
    projectName :: Name Project,
    projectWebUrl :: Url Project,
    projectDefaultBranch :: Maybe Ref,
    projectNamespacePath :: Path Rel Dir
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Project)

instance Validity Project

instance HasCodec Project where
  codec =
    object "Project" $
      Project
        <$> requiredField' "id"
          .= projectId
        <*> requiredField' "name"
          .= projectName
        <*> requiredField' "web_url"
          .= projectWebUrl
        <*> optionalField "default_branch" "default branch of the project. Will be missing if the project is empty"
          .= projectDefaultBranch
        <*> requiredField' "path_with_namespace"
          .= projectNamespacePath

type API = "projects" :> NamedRoutes ProjectAPI

data ProjectAPI mode = ProjectAPI
  { getAllProjects :: mode :- Get '[JSON] [Project],
    singleProject :: mode :- Capture "project ID" (Id Project) :> NamedRoutes SingleProjectAPI
  }
  deriving stock (Generic)

data SingleProjectAPI mode = SingleProjectAPI
  { getProject :: mode :- Get '[JSON] Project,
    jobs :: mode :- "jobs" :> NamedRoutes JobAPI
  }
  deriving stock (Generic)

data JobAPI mode = JobAPI
  { getSingleJob :: mode :- Capture "job ID" (Id Job.Job) :> Get '[JSON] Job.Job
  }
  deriving stock (Generic)
