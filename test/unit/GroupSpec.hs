{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module GroupSpec where

import Data.GenValidity.Path ()
import Group
import Lib
import Network.URI.Static
import Path.Posix
import Test.Syd
import Test.Syd.Aeson
import Test.Syd.Validity
import Test.Syd.Validity.Aeson
import Utils ()

spec :: Spec
spec = do
  jsonSpec @Group
  describe "golden tests" $ do
    it "full group" $ do
      let project = Group (Id 123) (Name "my group") (Url $$(staticURI "https://my.gitlab.com/my-group")) [reldir|my-team/my-project|]
      pureGoldenJSONValueFile "test/resources/group/group.json" project

instance GenValid Group
