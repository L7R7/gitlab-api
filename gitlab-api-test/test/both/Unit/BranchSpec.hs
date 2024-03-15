{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unit.BranchSpec where

import Data.GenValidity.Path ()
import Data.GenValidity.Time ()
import Gitlab.Branch
import Gitlab.Lib
import Network.URI.Static
import Test.Syd
import Test.Syd.Aeson
import Test.Syd.Validity
import Test.Syd.Validity.Aeson
import Utils (parseTime)

spec :: Spec
spec = do
  jsonSpec @Branch
  describe "golden tests" $ do
    it "full branch" $ do
      let branch =
            Branch
              "my-branch"
              True
              False
              True
              (Url $$(staticURI "https://my.gitlab.com/my-project/my-branch"))
              (Commit (parseTime "2024-02-29"))
      pureGoldenJSONValueFile "test/resources/branch/branch.json" branch

instance GenValid Branch

instance GenValid Commit
