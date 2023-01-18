{-# LANGUAGE DataKinds #-}

module Meta (API, MetaAPI (..), Version (..), Metadata (..)) where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Text
import Data.Validity
import Data.Validity.Text ()
import GHC.Generics
import Servant

type API = NamedRoutes MetaAPI

data MetaAPI mode = MetaAPI
  { version :: mode :- "version" :> Get '[JSON] Version,
    metadata :: mode :- "metadata" :> Get '[JSON] Metadata
  }
  deriving stock (Generic)

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
