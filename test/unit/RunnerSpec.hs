{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module RunnerSpec where

import Data.GenValidity.Path ()
import Data.GenValidity.Text ()
import Lib
import Runner
import Test.Syd hiding (runner)
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
              (IpAddress "127.0.0.1")
              True
              False
              False
              (RunnerType "instance_type")
              True
              (RunnerStatus "online")
      pureGoldenJSONValueFile "test/resources/runner/runner.json" runner

instance GenValid Runner

deriving newtype instance GenValid Description

deriving newtype instance GenValid IpAddress

deriving newtype instance GenValid RunnerType

deriving newtype instance GenValid RunnerStatus
