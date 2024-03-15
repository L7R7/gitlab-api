module Gitlab.Branch (Branch (..), Commit (..)) where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Text qualified as T
import Data.Time
import Data.Validity
import Data.Validity.Text ()
import Data.Validity.Time ()
import GHC.Generics
import Gitlab.Lib (Url)

data Branch = Branch
  { branchName :: T.Text,
    branchMerged :: Bool,
    branchProtected :: Bool,
    branchDefault :: Bool,
    branchWebUrl :: Url Branch,
    branchCommit :: Commit
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Branch)

instance Validity Branch

instance HasCodec Branch where
  codec =
    object "Branch" $
      Branch
        <$> requiredField' "name"
          .= branchName
        <*> requiredField' "merged"
          .= branchMerged
        <*> requiredField' "protected"
          .= branchProtected
        <*> requiredField' "default"
          .= branchDefault
        <*> requiredField' "web_url"
          .= branchWebUrl
        <*> requiredField' "commit"
          .= branchCommit

newtype Commit = Commit {commitCommittedDate :: UTCTime}
  deriving stock (Eq, Show, Generic)

instance Validity Commit

instance HasCodec Commit where
  codec = object "Commit" $ Commit <$> requiredField' "committed_date" .= commitCommittedDate
