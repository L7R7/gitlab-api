module Gitlab.MergeRequest (MergeRequest (..), DetailedMergeStatus (..)) where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.List.NonEmpty
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
    mergeRequestDescription :: Maybe Text,
    mergeRequestWip :: Bool,
    mergeRequestConflicts :: Bool,
    mergeRequestDetailedMergeStatus :: DetailedMergeStatus,
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
        <*> optionalFieldOrNull' "description"
          .= mergeRequestDescription
        <*> requiredField' "work_in_progress"
          .= mergeRequestWip
        <*> requiredField' "has_conflicts"
          .= mergeRequestConflicts
        <*> requiredField' "detailed_merge_status"
          .= mergeRequestDetailedMergeStatus
        <*> requiredField' "created_at"
          .= mergeRequestCreatedAt
        <*> requiredField' "web_url"
          .= mergeRequestWebUrl

data DetailedMergeStatus
  = ApprovalsSyncing
  | BlockedStatus
  | Checking
  | CIMustPass
  | CIStillRunning
  | Conflict
  | DiscussionsNotResolved
  | DraftStatus
  | ExternalStatusChecks
  | JiraAssociationMissing
  | Mergeable
  | NeedRebase
  | NotApproved
  | NotOpen
  | RequestedChanges
  | Unchecked
  deriving stock (Bounded, Enum, Eq, Show, Generic)

instance HasCodec DetailedMergeStatus where
  codec = stringConstCodec ((\js -> (js, detailedMergeRequestStatusToApiRepresentation js)) <$> (minBound :| [(succ minBound) .. maxBound]))

detailedMergeRequestStatusToApiRepresentation :: DetailedMergeStatus -> Text
detailedMergeRequestStatusToApiRepresentation ApprovalsSyncing = "approvals_syncing"
detailedMergeRequestStatusToApiRepresentation BlockedStatus = "blocked_status"
detailedMergeRequestStatusToApiRepresentation Checking = "checking"
detailedMergeRequestStatusToApiRepresentation CIMustPass = "ci_must_pass"
detailedMergeRequestStatusToApiRepresentation CIStillRunning = "ci_still_running"
detailedMergeRequestStatusToApiRepresentation Conflict = "conflict"
detailedMergeRequestStatusToApiRepresentation DiscussionsNotResolved = "discussions_not_resolved"
detailedMergeRequestStatusToApiRepresentation DraftStatus = "draft_status"
detailedMergeRequestStatusToApiRepresentation ExternalStatusChecks = "external_status_checks"
detailedMergeRequestStatusToApiRepresentation JiraAssociationMissing = "jira_association_missing"
detailedMergeRequestStatusToApiRepresentation Mergeable = "mergeable"
detailedMergeRequestStatusToApiRepresentation NeedRebase = "need_rebase"
detailedMergeRequestStatusToApiRepresentation NotApproved = "not_approved"
detailedMergeRequestStatusToApiRepresentation NotOpen = "not_open"
detailedMergeRequestStatusToApiRepresentation RequestedChanges = "requested_changes"
detailedMergeRequestStatusToApiRepresentation Unchecked = "unchecked"

instance Validity DetailedMergeStatus
