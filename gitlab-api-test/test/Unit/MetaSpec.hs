{-# OPTIONS_GHC -Wno-orphans #-}

module Unit.MetaSpec where

import Data.GenValidity.Path ()
import Gitlab.Meta
import Test.Syd
import Test.Syd.Aeson
import Test.Syd.Validity
import Test.Syd.Validity.Aeson
import Utils ()

spec :: Spec
spec = do
  describe "version" $ do
    jsonSpec @Version
    describe "golden tests" $ do
      it "version" $ do
        let version = Version "15.3.2" "e5a7085d434"
        pureGoldenJSONValueFile "test_resources/meta/version.json" version
  describe "metadata" $ do
    jsonSpec @Metadata
    describe "golden tests" $ do
      it "metadata" $ do
        let metadata = Metadata "15.3.2" "e5a7085d434"
        pureGoldenJSONValueFile "test_resources/meta/metadata.json" metadata

instance GenValid Version

instance GenValid Metadata
