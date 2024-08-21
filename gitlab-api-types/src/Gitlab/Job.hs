module Gitlab.Job (Job (..), JobStatus (..), Tag (..), Stage (..)) where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.List.NonEmpty
import Data.Text
import Data.Time
import Data.Validity
import Data.Validity.Text ()
import Data.Validity.Time ()
import GHC.Generics
import Gitlab.Lib

data Job = Job
  { jobId :: Id Job,
    jobStatus :: JobStatus,
    jobStage :: Stage,
    jobName :: Name Job,
    jobRef :: Ref,
    jobTag :: Bool,
    jobAllowFailure :: Bool,
    jobCreatedAt :: UTCTime,
    jobStartedAt :: UTCTime,
    jobFinishedAt :: UTCTime,
    jobDuration :: Duration,
    jobQueuedDuration :: Duration,
    jobFailureReason :: Maybe Text,
    jobWebUrl :: Url Job,
    jobTagList :: [Tag]
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Job)

instance HasCodec Job where
  codec =
    object "Job" $
      Job
        <$> requiredField' "id"
          .= jobId
        <*> requiredField' "status"
          .= jobStatus
        <*> requiredField' "stage"
          .= jobStage
        <*> requiredField' "name"
          .= jobName
        <*> requiredField' "ref"
          .= jobRef
        <*> requiredField' "tag"
          .= jobTag
        <*> requiredField' "allow_failure"
          .= jobAllowFailure
        <*> requiredField' "created_at"
          .= jobCreatedAt
        <*> requiredField' "started_at"
          .= jobStartedAt
        <*> requiredField' "finished_at"
          .= jobFinishedAt
        <*> requiredField' "duration"
          .= jobDuration
        <*> requiredField' "queued_duration"
          .= jobQueuedDuration
        <*> optionalField' "failure_reason"
          .= jobFailureReason
        <*> requiredField' "web_url"
          .= jobWebUrl
        <*> requiredField' "tag_list"
          .= jobTagList

instance Validity Job

data JobStatus
  = Unknown
  | Cancelled
  | Created
  | Failed
  | Manual
  | Pending
  | Preparing
  | Running
  | Scheduled
  | Skipped
  | Successful
  | SuccessfulWithWarnings
  | WaitingForResource
  deriving stock (Bounded, Enum, Eq, Ord, Show, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec JobStatus)

instance HasCodec JobStatus where
  codec = stringConstCodec ((\js -> (js, jobStatusToApiString js)) <$> (minBound :| [(succ minBound) .. maxBound]))

jobStatusToApiString :: JobStatus -> Text
jobStatusToApiString Unknown = "unknown"
jobStatusToApiString Cancelled = "canceled"
jobStatusToApiString Created = "created"
jobStatusToApiString Failed = "failed"
jobStatusToApiString Manual = "manual"
jobStatusToApiString Pending = "pending"
jobStatusToApiString Preparing = "preparing"
jobStatusToApiString Running = "running"
jobStatusToApiString Scheduled = "scheduled"
jobStatusToApiString Skipped = "skipped"
jobStatusToApiString Successful = "success"
jobStatusToApiString SuccessfulWithWarnings = "success-with-warnings"
jobStatusToApiString WaitingForResource = "waiting_for_resource"

instance Validity JobStatus

newtype Tag = Tag Text
  deriving newtype (Eq, Show)
  deriving stock (Generic)

instance Validity Tag where
  validate (Tag t) = check (t /= "") "must not be empty"

instance HasCodec Tag where
  codec = dimapCodec Tag (\(Tag txt) -> txt) codec

newtype Stage = Stage Text
  deriving newtype (Eq, Show)
  deriving stock (Generic)

instance Validity Stage where
  validate (Stage t) = check (t /= "") "must not be empty"

instance HasCodec Stage where
  codec = dimapCodec Stage (\(Stage txt) -> txt) codec
