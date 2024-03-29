module Gitlab.Meta (Version (..), Metadata (..)) where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Text
import Data.Validity
import Data.Validity.Text ()
import GHC.Generics

data Version = Version
  { vVersion :: Text,
    vRevision :: Text
  }
  deriving stock (Generic, Eq, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec Version)

instance Validity Version

instance HasCodec Version where
  codec =
    object "Version" $
      Version
        <$> requiredField' "version"
          .= vVersion
        <*> requiredField' "revision"
          .= vRevision

data Metadata = Metadata
  { mVersion :: Text,
    mRevision :: Text
  }
  deriving stock (Generic, Eq, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec Metadata)

instance Validity Metadata

instance HasCodec Metadata where
  codec =
    object "Metadata" $
      Metadata
        <$> requiredField' "version"
          .= mVersion
        <*> requiredField' "revision"
          .= mRevision
