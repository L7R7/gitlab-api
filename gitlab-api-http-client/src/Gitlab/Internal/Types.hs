module Gitlab.Internal.Types
  ( BaseUrl (..),
    ApiToken (..),
    StatusCode,
    UpdateError (..),
  )
where

import Control.Exception.Base (SomeException)
import Data.Aeson hiding (Value)
import Data.ByteString.Lazy qualified as L
import Data.Text (Text)
import Network.HTTP.Simple
import Network.URI (URI)

newtype BaseUrl = BaseUrl URI deriving newtype (Show)

newtype ApiToken = ApiToken Text deriving newtype (FromJSON, Show)

type StatusCode = Int

data UpdateError
  = HttpError HttpException
  | ResponseWasNotSuccessful StatusCode L.ByteString
  | ExceptionError SomeException
  | ConversionError JSONException
  | ConversionError' String
  | ParseUrlError Text
  deriving stock (Show)
