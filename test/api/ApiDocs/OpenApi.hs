{-# OPTIONS_GHC -Wno-orphans #-}

module ApiDocs.OpenApi () where

import Autodocodec
import Autodocodec.OpenAPI ()
import Data.OpenApi (ToParamSchema (..), ToSchema (..))
import qualified Job
import Lib
import qualified Meta
import qualified Project

instance ToParamSchema (Id a) where
  toParamSchema _ = mempty -- todo

deriving via (Autodocodec Meta.Version) instance (ToSchema Meta.Version)

deriving via (Autodocodec Meta.Metadata) instance (ToSchema Meta.Metadata)

deriving via (Autodocodec Project.Project) instance (ToSchema Project.Project)

deriving via (Autodocodec Job.Job) instance (ToSchema Job.Job)
