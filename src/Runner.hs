module Runner (Runner (..), Description (..), IpAddress (..), RunnerType (..), RunnerStatus (..)) where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Text
import Data.Validity
import GHC.Generics (Generic)
import Lib

-- | runner representation for the list of runners
data Runner = Runner
  { runnerId :: Id Runner,
    runnerName :: Name Runner,
    runnerDescription :: Description,
    runnerIpAddress :: IpAddress,
    runnerActive :: Bool,
    runnerPaused :: Bool,
    runnerShared :: Bool,
    runnerType :: RunnerType,
    runnerOnline :: Bool,
    runnerStatus :: RunnerStatus
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Runner)

instance Validity Runner

instance HasCodec Runner where
  codec =
    object "Runner" $
      Runner
        <$> requiredField' "id" .= runnerId
        <*> requiredField' "name" .= runnerName
        <*> requiredField' "description" .= runnerDescription
        <*> requiredField' "ip_address" .= runnerIpAddress
        <*> requiredField' "active" .= runnerActive
        <*> requiredField' "paused" .= runnerPaused
        <*> requiredField' "is_shared" .= runnerShared
        <*> requiredField' "runner_type" .= runnerType
        <*> requiredField' "online" .= runnerOnline
        <*> requiredField' "status" .= runnerStatus

newtype Description = Description Text
  deriving newtype (Eq, Show)

instance Validity Description where
  validate _ = valid

instance HasCodec Description where
  codec = dimapCodec Description (\(Description txt) -> txt) codec

newtype IpAddress = IpAddress Text
  deriving newtype (Eq, Show)

instance Validity IpAddress where
  validate _ = valid

instance HasCodec IpAddress where
  codec = dimapCodec IpAddress (\(IpAddress txt) -> txt) codec

-- | todo: model the possibilities explicitly
newtype RunnerType = RunnerType Text
  deriving newtype (Eq, Show)

instance Validity RunnerType where
  validate _ = valid

instance HasCodec RunnerType where
  codec = dimapCodec RunnerType (\(RunnerType txt) -> txt) codec

-- | todo: model the possibilities explicitly
newtype RunnerStatus = RunnerStatus Text
  deriving newtype (Eq, Show)

instance Validity RunnerStatus where
  validate _ = valid

instance HasCodec RunnerStatus where
  codec = dimapCodec RunnerStatus (\(RunnerStatus txt) -> txt) codec
