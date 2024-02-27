module Gitlab.Package (Package (..), PackageLinks (..)) where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Text
import Data.Validity (Validity)
import Data.Validity.Text ()
import GHC.Generics (Generic)
import Gitlab.Lib

data Package = Package
  { packageId :: Id Package,
    packageName :: Name Package,
    packageVersion :: Text,
    packageType :: Text,
    packageLinks :: PackageLinks
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Package)

instance Validity Package

instance HasCodec Package where
  codec =
    object "Package" $
      Package
        <$> requiredField' "id" .= packageId
        <*> requiredField' "name" .= packageName
        <*> requiredField' "version" .= packageVersion
        <*> requiredField' "package_type" .= packageType
        <*> requiredField' "_links" .= packageLinks

data WebPath

newtype PackageLinks = PackageLinks
  { packageLinksWebPath :: Url WebPath
  }
  deriving stock (Eq, Show, Generic)

instance Validity PackageLinks

instance HasCodec PackageLinks where
  codec = object "PackageLinks" $ PackageLinks <$> requiredField' "web_path" .= packageLinksWebPath
