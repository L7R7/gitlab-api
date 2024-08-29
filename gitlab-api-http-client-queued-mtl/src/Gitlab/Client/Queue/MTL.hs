module Gitlab.Client.Queue.MTL
  ( fetchDataQueued,
    GCQ.QueueConfig(..),
    GC.ApiToken (..),
    GC.BaseUrl (..),
    GC.UpdateError (..),
    GCM.HasApiToken (..),
    GCM.HasBaseUrl (..),
  )
where

import Burrito
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Aeson (FromJSON)
import Data.Text
import Gitlab.Client qualified as GC
import Gitlab.Client.MTL qualified as GCM
import Gitlab.Client.Queue qualified as GCQ

fetchDataQueued ::
  forall a b m.
  (FromJSON a, MonadUnliftIO m, MonadMask m, GCM.HasApiToken m, GCM.HasBaseUrl m) =>
  Template ->
  [(String, Value)] ->
  GCQ.QueueConfig ->
  (a -> m (Either GC.UpdateError ([Text], b))) ->
  m (Either GC.UpdateError [b])
fetchDataQueued template vars queueConfig processFunction = do
  baseUrl <- GCM.getBaseUrl
  apiToken <- GCM.getApiToken
  GCQ.fetchDataQueued baseUrl apiToken template vars queueConfig processFunction
