{-# LANGUAGE DataKinds #-}

module Gitlab.Group (Group (..)) where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Validity
import Data.Validity.Path ()
import Gitlab.Lib (Id, Name, Url)
import Path
import Gitlab.Project qualified
import Servant
import Servant.API.Generic

data Group = Group
  { groupId :: Id Group,
    groupName :: Name Group,
    groupWebUrl :: Url Group,
    groupFullPath :: Path Rel Dir
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Group)

instance Validity Group

instance HasCodec Group where
  codec =
    object "Group" $
      Group
        <$> requiredField' "id"
          .= groupId
        <*> requiredField' "name"
          .= groupName
        <*> requiredField' "web_url"
          .= groupWebUrl
        <*> requiredField' "full_path"
          .= groupFullPath
