{-# OPTIONS_GHC -Wno-orphans #-}

module Api.ApiDocs.OpenApi () where

import Autodocodec
import Autodocodec.OpenAPI ()
import Data.OpenApi (ToParamSchema (..), ToSchema (..))
import Gitlab.Branch
import Gitlab.Group
import Gitlab.Job
import Gitlab.Lib
import Gitlab.MergeRequest
import Gitlab.Meta
import Gitlab.Project

instance ToParamSchema (Id a) where
  toParamSchema _ = mempty -- todo

deriving via (Autodocodec Version) instance (ToSchema Version)

deriving via (Autodocodec Metadata) instance (ToSchema Metadata)

deriving via (Autodocodec Project) instance (ToSchema Project)

deriving via (Autodocodec Job) instance (ToSchema Job)

deriving via (Autodocodec Group) instance (ToSchema Group)

deriving via (Autodocodec MergeRequest) instance (ToSchema MergeRequest)

deriving via (Autodocodec Branch) instance (ToSchema Branch)
