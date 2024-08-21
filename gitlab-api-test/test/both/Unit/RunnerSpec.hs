{-# OPTIONS_GHC -Wno-orphans #-}

module Unit.RunnerSpec where

import Data.GenValidity.Path ()
import Data.GenValidity.Text ()
import Gitlab.Lib
import Gitlab.Runner
import Test.Syd
import Test.Syd.Aeson
import Test.Syd.Validity
import Test.Syd.Validity.Aeson
import Utils ()

spec :: Spec
spec = do
  jsonSpec @Runner
  describe "golden tests" $ do
    it "full runner" $ do
      let runner =
            Runner
              (Id 23)
              (Name "my-runner")
              (Description "this is my runner")
              (Just $ IpAddress "127.0.0.1")
              True
              False
              False
              (RunnerType "instance_type")
              [RunnerTag "foo", RunnerTag "bar"]
              True
              (RunnerStatus "online")
      pureGoldenJSONValueFile "test/resources/runner/full.json" runner
    it "no ip address" $ do
      let runner =
            Runner
              (Id 23)
              (Name "my-runner")
              (Description "this is my runner")
              Nothing
              True
              False
              False
              (RunnerType "instance_type")
              [RunnerTag "foo", RunnerTag "bar"]
              True
              (RunnerStatus "online")
      pureGoldenJSONValueFile "test/resources/runner/no-ip.json" runner

instance GenValid Runner

deriving newtype instance GenValid Description

deriving newtype instance GenValid IpAddress

deriving newtype instance GenValid RunnerType

deriving newtype instance GenValid RunnerStatus

deriving newtype instance GenValid RunnerTag
