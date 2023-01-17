{-# OPTIONS_GHC -Wno-orphans #-}

module MetaSpec where

import Data.GenValidity.Path ()
import Meta
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
        pureGoldenJSONValueFile "test/resources/meta/version.json" version
  describe "metadata" $ do
    jsonSpec @Metadata
    describe "golden tests" $ do
      it "metadata" $ do
        let metadata = Metadata "15.3.2" "e5a7085d434"
        pureGoldenJSONValueFile "test/resources/meta/metadata.json" metadata

instance GenValid Version

instance GenValid Metadata
