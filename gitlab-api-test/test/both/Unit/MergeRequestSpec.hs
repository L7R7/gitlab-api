{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unit.MergeRequestSpec where

import Data.GenValidity.Path ()
import Data.GenValidity.Time ()
import Gitlab.Lib
import Gitlab.MergeRequest
import Network.URI.Static
import Test.Syd
import Test.Syd.Aeson
import Test.Syd.Validity
import Test.Syd.Validity.Aeson
import Utils (parseTime)

spec :: Spec
spec = do
  jsonSpec @MergeRequest
  describe "golden tests" $ do
    it "full merge request" $ do
      let mergeRequest =
            MergeRequest
              (Id 77)
              (Id 123)
              "MR Title"
              "MR Description"
              True
              False
              (parseTime "2024-02-29")
              (Url $$(staticURI "https://my.gitlab.com/my-merge-request"))
      pureGoldenJSONValueFile "test/resources/merge-request/merge-request.json" mergeRequest

instance GenValid MergeRequest
