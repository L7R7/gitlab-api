{-# OPTIONS_GHC -Wno-orphans #-}

module Gitlab.Lib (Id (..), Url (..), Ref (..), Name (..), Duration (..)) where

import Autodocodec
import Data.Either.Combinators
import Data.Scientific
import Data.Text
import Data.Validity
import Data.Validity.URI ()
import GHC.Generics
import Network.URI
import Path
import Servant (FromHttpApiData, ToHttpApiData)

newtype Id a = Id Int
  deriving newtype (Eq, Show, FromHttpApiData, ToHttpApiData)
  deriving stock (Generic)

instance Validity (Id a) where
  validate (Id i) = check (i >= 0) "must not be negative"

instance HasCodec (Id a) where
  codec = dimapCodec Id (\(Id i) -> i) codec

newtype Url a = Url URI
  deriving newtype (Eq, Show, Validity)

instance HasCodec (Url a) where
  codec = dimapCodec Url (\(Url uri) -> uri) codec

instance HasCodec URI where
  codec = bimapCodec (maybeToRight "can't parse URI" . parseURI) show stringCodec

newtype Ref = Ref Text
  deriving newtype (Eq, Show)
  deriving stock (Generic)

instance Validity Ref where
  validate (Ref t) = check (t /= "") "must not be empty"

instance HasCodec Ref where
  codec = dimapCodec Ref (\(Ref txt) -> txt) codec

newtype Name a = Name Text
  deriving newtype (Eq, Show)
  deriving stock (Generic)

instance Validity (Name a) where
  validate (Name t) = check (t /= "") "must not be empty"

instance HasCodec (Name a) where
  codec = dimapCodec Name (\(Name txt) -> txt) codec

instance HasCodec (Path Rel Dir) where
  codec = codecViaAeson "Path"

newtype Duration = Duration {getDuration :: Scientific}
  deriving newtype (Eq, Show)
  deriving stock (Generic)

instance Validity Duration

instance HasCodec Duration where
  codec = dimapCodec Duration getDuration scientificCodec
