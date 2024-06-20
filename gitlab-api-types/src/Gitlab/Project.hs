module Gitlab.Project (Project (..), MergeMethod (..)) where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.List.NonEmpty
import Data.Text (Text)
import Data.Validity
import Data.Validity.Path ()
import Data.Validity.Text ()
import GHC.Generics
import Gitlab.Lib (EnabledDisabled, Id, Name, Ref, Url)
import Path

data Project = Project
  { projectId :: Id Project,
    projectName :: Name Project,
    projectWebUrl :: Url Project,
    projectDefaultBranch :: Maybe Ref,
    projectMergeRequestsEnabled :: Bool,
    projectMergeMethod :: MergeMethod,
    projectPath :: Text,
    projectPathWithNamespace :: Path Rel Dir,
    projectRemoveSourceBranchAfterMerge :: Maybe Bool,
    projectOnlyAllowMergeIfPipelineSucceeds :: Maybe Bool,
    projectOnlyAllowMergeIfAllDiscussionsAreResolved :: Maybe Bool,
    projectAutoCancelPendingPipelines :: Maybe EnabledDisabled,
    projectSshUrlToRepo :: Text
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
        <*> requiredField' "merge_requests_enabled"
          .= projectMergeRequestsEnabled
        <*> requiredField' "merge_method"
          .= projectMergeMethod
        <*> requiredField' "path"
          .= projectPath
        <*> requiredField' "path_with_namespace"
          .= projectPathWithNamespace
        <*> requiredField "remove_source_branch_after_merge" "Project-wide default value for this option in MRs. Is `null` when the user has not set it manually."
          .= projectRemoveSourceBranchAfterMerge
        <*> requiredField "only_allow_merge_if_pipeline_succeeds" "Project-wide default value for this option in MRs. Is `null` when the user has not set it manually."
          .= projectOnlyAllowMergeIfPipelineSucceeds
        <*> requiredField "only_allow_merge_if_all_discussions_are_resolved" "Project-wide default value for this option in MRs. Is `null` when the user has not set it manually."
          .= projectOnlyAllowMergeIfAllDiscussionsAreResolved
        <*> optionalField' "auto_cancel_pending_pipelines"
          .= projectAutoCancelPendingPipelines
        <*> requiredField' "ssh_url_to_repo"
          .= projectSshUrlToRepo

data MergeMethod = Merge | RebaseMerge | FastForward
  deriving stock (Eq, Show, Generic)

instance HasCodec MergeMethod where
  codec = stringConstCodec $ (Merge, "merge") :| [(RebaseMerge, "rebase_merge"), (FastForward, "ff")]

instance Validity MergeMethod
