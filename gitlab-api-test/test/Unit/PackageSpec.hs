{-# OPTIONS_GHC -Wno-orphans #-}

module Unit.PackageSpec where

import Data.GenValidity.Path ()
import Gitlab.Lib
import Gitlab.Package
import Test.Syd
import Test.Syd.Aeson
import Test.Syd.Validity
import Test.Syd.Validity.Aeson
import Utils ()

spec :: Spec
spec = do
  jsonSpec @Package
  describe "golden tests" $ do
    it "Package" $ do
      let package = Package (Id 12) (Name "my-package") "1.2.3" "maven" (PackageLinks "/my-group/my-package")
      pureGoldenJSONValueFile "test_resources/package/package.json" package

instance GenValid Package

instance GenValid PackageLinks
