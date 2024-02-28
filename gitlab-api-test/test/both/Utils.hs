{-# OPTIONS_GHC -Wno-orphans #-}

module Utils where

import Data.GenValidity.Scientific ()
import Data.GenValidity.Text ()
import Gitlab.Lib
import Network.URI
import Test.Syd.Validity

instance GenValid (Name a)

instance GenValid (Id a)

instance GenValid (Url a) where
  genValid = pure $ Url $ URI "https:" (Just (URIAuth "" "www.gitlab.com" "")) "/test/test" "" ""
  shrinkValid _ = []

instance GenValid Duration

instance GenValid EnabledDisabled
