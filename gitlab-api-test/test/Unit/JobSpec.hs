{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unit.JobSpec where

import Data.GenValidity.Path ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.Time
import Gitlab.Job
import Gitlab.Lib
import Network.URI.Static
import Test.Syd
import Test.Syd.Aeson
import Test.Syd.Validity
import Test.Syd.Validity.Aeson
import Utils ()

spec :: Spec
spec = do
  jsonSpec @Job
  describe "golden tests" $ do
    it "full job (no failure reason)" $
      do
        let job =
              Job
                (Id 456)
                Successful
                (Stage "build")
                (Name "compile")
                (Ref "main")
                True
                False
                (UTCTime (fromGregorian 2023 1 10) (secondsToDiffTime 0))
                (UTCTime (fromGregorian 2023 1 11) (secondsToDiffTime 0))
                (UTCTime (fromGregorian 2023 1 12) (secondsToDiffTime 0))
                (Duration 45.4)
                (Duration 12.6)
                Nothing
                (Url $$(staticURI "https://my.gitlab.com/my-group/my-project/-/jobs/4028634"))
                [Tag "my-tag", Tag "my-other-tag"]
        pureGoldenJSONValueFile "test_resources/job/no-failure-reason.json" job
    it "full job (with failure reason)" $
      do
        let job =
              Job
                (Id 456)
                Failed
                (Stage "build")
                (Name "compile")
                (Ref "main")
                True
                False
                (UTCTime (fromGregorian 2023 1 10) (secondsToDiffTime 0))
                (UTCTime (fromGregorian 2023 1 11) (secondsToDiffTime 0))
                (UTCTime (fromGregorian 2023 1 12) (secondsToDiffTime 0))
                (Duration 45.4)
                (Duration 12.6)
                (Just "it's broken")
                (Url $$(staticURI "https://my.gitlab.com/my-group/my-project/-/jobs/4028634"))
                [Tag "my-tag", Tag "my-other-tag"]
        pureGoldenJSONValueFile "test_resources/job/with-failure-reason.json" job

instance GenValid Job

instance GenValid JobStatus

instance GenValid Stage

instance GenValid Ref

instance GenValid Tag
