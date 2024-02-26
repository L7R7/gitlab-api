{-# LANGUAGE DataKinds #-}

module Gitlab.Project (Project (..)) where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Validity
import Data.Validity.Path ()
import Gitlab.Job qualified
import Gitlab.Lib (Id, Name, Ref, Url)
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
