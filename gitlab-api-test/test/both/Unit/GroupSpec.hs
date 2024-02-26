{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unit.GroupSpec where

import Data.GenValidity.Path ()
import Gitlab.Group
import Gitlab.Lib
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
      let group = Group (Id 123) (Name "my group") (Url $$(staticURI "https://my.gitlab.com/my-group")) [reldir|my-team/my-group|]
      pureGoldenJSONValueFile "test/resources/group/group.json" group

instance GenValid Group
