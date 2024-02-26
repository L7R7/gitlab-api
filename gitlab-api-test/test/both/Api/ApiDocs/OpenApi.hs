{-# OPTIONS_GHC -Wno-orphans #-}

module Api.ApiDocs.OpenApi () where

import Autodocodec
import Autodocodec.OpenAPI ()
import Data.OpenApi (ToParamSchema (..), ToSchema (..))
import Gitlab.Group qualified
import Gitlab.Job qualified
import Gitlab.Lib
import Gitlab.Meta qualified
import Gitlab.Project qualified

instance ToParamSchema (Id a) where
  toParamSchema _ = mempty -- todo

deriving via (Autodocodec Gitlab.Meta.Version) instance (ToSchema Gitlab.Meta.Version)

deriving via (Autodocodec Gitlab.Meta.Metadata) instance (ToSchema Gitlab.Meta.Metadata)

deriving via (Autodocodec Gitlab.Project.Project) instance (ToSchema Gitlab.Project.Project)

deriving via (Autodocodec Gitlab.Job.Job) instance (ToSchema Gitlab.Job.Job)

deriving via (Autodocodec Gitlab.Group.Group) instance (ToSchema Gitlab.Group.Group)
