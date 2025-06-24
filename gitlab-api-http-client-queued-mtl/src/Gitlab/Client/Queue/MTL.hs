module Gitlab.Client.Queue.MTL
  ( fetchDataQueued,
    GCQ.QueueConfig (..),
    GCQ.ProcessResult (..),
    GC.ApiToken (..),
    GC.BaseUrl (..),
    GC.UpdateError (..),
    GCM.HasApiToken (..),
    GCM.HasUserAgent (..),
    GCM.HasBaseUrl (..),
  )
where

import Burrito
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Aeson (FromJSON)
import Gitlab.Client qualified as GC
import Gitlab.Client.MTL qualified as GCM
import Gitlab.Client.Queue qualified as GCQ

fetchDataQueued ::
  forall a b m.
  (FromJSON a, MonadUnliftIO m, MonadMask m, GCM.HasApiToken m, GCM.HasUserAgent m, GCM.HasBaseUrl m) =>
  Template ->
  [(String, Value)] ->
  GCQ.QueueConfig ->
  (a -> m (Either GC.UpdateError (GCQ.ProcessResult b))) ->
  m (Either GC.UpdateError [b])
fetchDataQueued template vars queueConfig processFunction = do
  baseUrl <- GCM.getBaseUrl
  userAgent <- GCM.getUserAgent
  apiToken <- GCM.getApiToken
  GCQ.fetchDataQueued baseUrl apiToken userAgent template vars queueConfig processFunction
