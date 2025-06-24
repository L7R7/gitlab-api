module Gitlab.Client.MTL
  ( fetchData,
    fetchData',
    fetchDataPaginated,
    headRequest,
    GC.ApiToken (..),
    GC.UserAgent (..),
    GC.BaseUrl (..),
    GC.UpdateError (..),
    HasApiToken (..),
    HasUserAgent (..),
    HasBaseUrl (..),
  )
where

import Burrito
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (FromJSON)
import Gitlab.Client qualified as GC
import Network.HTTP.Simple
import Network.HTTP.Types (Status)

class HasApiToken m where
  getApiToken :: m GC.ApiToken

class HasUserAgent m where
  getUserAgent :: m GC.UserAgent

class HasBaseUrl m where
  getBaseUrl :: m GC.BaseUrl

fetchData :: (FromJSON a, HasApiToken m, HasUserAgent m, HasBaseUrl m, MonadIO m) => Template -> [(String, Value)] -> m (Either GC.UpdateError a)
fetchData template vars = do
  token <- getApiToken
  userAgent <- getUserAgent
  host <- getBaseUrl
  liftIO $ GC.fetchData host token userAgent template vars

fetchData' :: (FromJSON a, HasApiToken m, HasUserAgent m, HasBaseUrl m, MonadIO m) => (Request -> Request) -> Template -> [(String, Value)] -> m (Either GC.UpdateError a)
fetchData' reqTransformer template vars = do
  token <- getApiToken
  userAgent <- getUserAgent
  host <- getBaseUrl
  liftIO $ GC.fetchData' host token userAgent reqTransformer template vars

fetchDataPaginated :: (FromJSON a, HasApiToken m, HasUserAgent m, HasBaseUrl m, MonadIO m) => Template -> [(String, Value)] -> m (Either GC.UpdateError [a])
fetchDataPaginated template vars = do
  token <- getApiToken
  userAgent <- getUserAgent
  host <- getBaseUrl
  liftIO $ GC.fetchDataPaginated token userAgent host template vars

headRequest :: (HasApiToken m, HasUserAgent m, HasBaseUrl m, MonadIO m) => (Request -> Request) -> Template -> [(String, Value)] -> m (Either GC.UpdateError Status)
headRequest reqTransformer template vars = do
  token <- getApiToken
  userAgent <- getUserAgent
  host <- getBaseUrl
  liftIO $ GC.headRequest host token userAgent reqTransformer template vars
