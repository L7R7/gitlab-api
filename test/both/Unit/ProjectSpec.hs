{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unit.ProjectSpec where

import Data.GenValidity.Path ()
import Lib
import Network.URI.Static
import Path.Posix
import Project
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
      let project = Project (Id 123) (Name "my project") (Url $$(staticURI "https://my.gitlab.com/pipelines/411")) (Just (Ref "main")) [reldir|my-team/my-project|]
      pureGoldenJSONValueFile "test/resources/project/project.json" project
    it "project without default branch" $ do
      let project = Project (Id 123) (Name "my project") (Url $$(staticURI "https://my.gitlab.com/pipelines/411")) Nothing [reldir|my-team/my-project|]
      pureGoldenJSONValueFile "test/resources/project/project-no-default-branch.json" project

instance GenValid Project

deriving newtype instance GenValid Ref
