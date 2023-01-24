{-# LANGUAGE DataKinds #-}

module Group (Group (..), API, GroupAPI (..), SingleGroupAPI (..)) where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Validity
import Data.Validity.Path ()
import Lib (Id, Name, Url)
import Path
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

type API = "groups" :> NamedRoutes GroupAPI

data GroupAPI mode = GroupAPI
  { getAllGroups :: mode :- Get '[JSON] [Group],
    singleGroup :: mode :- Capture "group ID" (Id Group) :> NamedRoutes SingleGroupAPI
  }
  deriving stock (Generic)

data SingleGroupAPI mode = SingleGroupAPI
  { getGroup :: mode :- Get '[JSON] Group
  }
  deriving stock (Generic)
