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
  | Checking
  | CIMustPass
  | CIStillRunning
  | CommitsStatus
  | Conflict
  | DiscussionsNotResolved
  | DraftStatus
  | JiraAssociationMissing
  | LockedPaths
  | LockedLfsFiles
  | Mergeable
  | MergeRequestBlocked
  | MergeTime
  | NeedRebase
  | NotApproved
  | NotOpen
  | Preparing
  | RequestedChanges
  | SecurityPolicyEvaluation
  | SecurityPolicyViolations
  | StatusCheckMustPass
  | Unchecked
  deriving stock (Bounded, Enum, Eq, Show, Generic)

instance HasCodec DetailedMergeStatus where
  codec = stringConstCodec ((\js -> (js, detailedMergeRequestStatusToApiRepresentation js)) <$> (minBound :| [(succ minBound) .. maxBound]))

detailedMergeRequestStatusToApiRepresentation :: DetailedMergeStatus -> Text
detailedMergeRequestStatusToApiRepresentation ApprovalsSyncing = "approvals_syncing"
detailedMergeRequestStatusToApiRepresentation Checking = "checking"
detailedMergeRequestStatusToApiRepresentation CIMustPass = "ci_must_pass"
detailedMergeRequestStatusToApiRepresentation CIStillRunning = "ci_still_running"
detailedMergeRequestStatusToApiRepresentation CommitsStatus = "commits_status"
detailedMergeRequestStatusToApiRepresentation Conflict = "conflict"
detailedMergeRequestStatusToApiRepresentation DiscussionsNotResolved = "discussions_not_resolved"
detailedMergeRequestStatusToApiRepresentation DraftStatus = "draft_status"
detailedMergeRequestStatusToApiRepresentation JiraAssociationMissing = "jira_association_missing"
detailedMergeRequestStatusToApiRepresentation LockedPaths = "locked_paths"
detailedMergeRequestStatusToApiRepresentation LockedLfsFiles = "locked_lfs_files"
detailedMergeRequestStatusToApiRepresentation Mergeable = "mergeable"
detailedMergeRequestStatusToApiRepresentation MergeRequestBlocked = "merge_request_blocked"
detailedMergeRequestStatusToApiRepresentation MergeTime = "merge_time"
detailedMergeRequestStatusToApiRepresentation NeedRebase = "need_rebase"
detailedMergeRequestStatusToApiRepresentation NotApproved = "not_approved"
detailedMergeRequestStatusToApiRepresentation NotOpen = "not_open"
detailedMergeRequestStatusToApiRepresentation Preparing = "preparing"
detailedMergeRequestStatusToApiRepresentation RequestedChanges = "requested_changes"
detailedMergeRequestStatusToApiRepresentation SecurityPolicyEvaluation = "security_policy_evaluation"
detailedMergeRequestStatusToApiRepresentation SecurityPolicyViolations = "security_policy_violations"
detailedMergeRequestStatusToApiRepresentation StatusCheckMustPass = "status_check_must_pass"
detailedMergeRequestStatusToApiRepresentation Unchecked = "unchecked"

instance Validity DetailedMergeStatus
