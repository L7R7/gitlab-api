{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unit.ProjectSpec where

import Data.GenValidity.Path ()
import Gitlab.Lib
import Gitlab.Project
import Network.URI.Static
import Path.Posix
import Test.Syd
import Test.Syd.Aeson
import Test.Syd.Validity
import Test.Syd.Validity.Aeson
import Utils ()

spec :: Spec
spec = do
  genValidSpec @(Url Int)
  shrinkValidSpec @(Url Int)
  jsonSpec @Project
  describe "golden tests" $ do
    it "full project" $ do
      let project =
            Project
              (Id 123)
              (Name "my project")
              (Url $$(staticURI "https://my.gitlab.com/pipelines/411"))
              (Just (Ref "main"))
              True
              FastForward
              [reldir|my-team/my-project|]
              (Just True)
              (Just False)
              (Just True)
              (Just Enabled)
              "git@my.gitlab.com:group/subgroup/my-project.git"
      pureGoldenJSONValueFile "test/resources/project/project.json" project

    it "project without optional fields" $ do
      let project =
            Project
              (Id 123)
              (Name "my project")
              (Url $$(staticURI "https://my.gitlab.com/pipelines/411"))
              Nothing
              True
              Merge
              [reldir|my-team/my-project|]
              Nothing
              Nothing
              Nothing
              Nothing
              "git@my.gitlab.com:group/subgroup/my-project.git"
      pureGoldenJSONValueFile "test/resources/project/project-no-optional-fields.json" project

instance GenValid Project

instance GenValid MergeMethod

deriving newtype instance GenValid Ref
