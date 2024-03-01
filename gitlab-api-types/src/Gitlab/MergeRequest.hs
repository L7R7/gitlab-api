module Gitlab.MergeRequest (MergeRequest (..)) where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Text
import Data.Time
import Data.Validity
import Data.Validity.Time ()
import GHC.Generics
import Gitlab.Lib (Id, Url)
import Gitlab.Project (Project)

data MergeRequest = MergeRequest
  { mergeRequestIid :: Id MergeRequest,
    mergeRequestProjectId :: Id Project,
    mergeRequestTitle :: Text,
    mergeRequestDescription :: Text,
    mergeRequestWip :: Bool,
    mergeRequestConflicts :: Bool,
    mergeRequestCreatedAt :: UTCTime,
    mergeRequestWebUrl :: Url MergeRequest
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec MergeRequest)

instance Validity MergeRequest

instance HasCodec MergeRequest where
  codec =
    object "MergeRequest" $
      MergeRequest
        <$> requiredField "iid" "NOTE: IID OF THE MR, NOT ID"
          .= mergeRequestIid
        <*> requiredField' "project_id"
          .= mergeRequestProjectId
        <*> requiredField' "title"
          .= mergeRequestTitle
        <*> requiredField' "description"
          .= mergeRequestDescription
        <*> requiredField' "work_in_progress"
          .= mergeRequestWip
        <*> requiredField' "has_conflicts"
          .= mergeRequestConflicts
        <*> requiredField' "created_at"
          .= mergeRequestCreatedAt
        <*> requiredField' "web_url"
          .= mergeRequestWebUrl
