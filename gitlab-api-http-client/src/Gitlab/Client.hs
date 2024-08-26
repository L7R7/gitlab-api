module Gitlab.Client
  ( BaseUrl (..),
    ApiToken (..),
    UpdateError (..),
    fetchData,
    fetchData',
    fetchDataPaginated,
    headRequest,
  )
where

import Burrito
import Control.Exception.Base (try)
import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson hiding (Value)
import Data.ByteString.Lazy qualified as L
import Data.Either.Combinators (mapLeft)
import Gitlab.Internal.Types
import Gitlab.Internal.Util
import Network.HTTP.Simple
import Network.HTTP.Types (Status, statusCode, statusIsSuccessful)
import Network.HTTP.Types.Header (hAccept)

fetchData :: (FromJSON a, MonadIO m) => BaseUrl -> ApiToken -> Template -> [(String, Value)] -> m (Either UpdateError a)
fetchData baseUrl apiToken = fetchData' baseUrl apiToken id

fetchData' :: forall a m. (FromJSON a, MonadIO m) => BaseUrl -> ApiToken -> RequestTransformer -> Template -> [(String, Value)] -> m (Either UpdateError a)
fetchData' baseUrl apiToken reqTransformer = doReq (fmap f . httpLBS) baseUrl apiToken (addRequestHeader hAccept "application/json" . reqTransformer)
  where
    f :: Response L.ByteString -> Either UpdateError a
    f res =
      if statusIsSuccessful (getResponseStatus res)
        then mapLeft ConversionError' (eitherDecode (getResponseBody res))
        else Left $ ResponseWasNotSuccessful (statusCode $ getResponseStatus res) (getResponseBody res)

fetchDataPaginated :: (FromJSON a, MonadIO m) => ApiToken -> BaseUrl -> Template -> [(String, Value)] -> m (Either UpdateError [a])
fetchDataPaginated apiToken baseUrl template vars =
  case createRequest baseUrl apiToken id template vars of
    Left invalidUrl -> pure $ Left invalidUrl
    Right request -> fetchDataPaginated' apiToken request []

fetchDataPaginated' :: (FromJSON a, MonadIO m) => ApiToken -> Request -> [a] -> m (Either UpdateError [a])
fetchDataPaginated' apiToken request acc = do
  result <- liftIO $ try $ do
    response <- httpJSONEither (setTimeout $ addToken apiToken request)
    let next = parseNextRequest response
    case mapLeft ConversionError $ getResponseBody response of
      Left err -> pure $ Left err
      Right as -> maybe (pure $ Right (as <> acc)) (\req -> fetchDataPaginated' apiToken req (as <> acc)) next
  pure $ mapLeft removeApiTokenFromUpdateError $ join $ mapLeft HttpError result

headRequest :: (MonadIO m) => BaseUrl -> ApiToken -> RequestTransformer -> Template -> [(String, Value)] -> m (Either UpdateError Status)
headRequest baseUrl apiToken reqTransformer = doReq (fmap (Right . getResponseStatus) . httpNoBody) baseUrl apiToken (reqTransformer . setRequestMethod "HEAD")

doReq :: (MonadIO m) => (Request -> IO (Either UpdateError a)) -> BaseUrl -> ApiToken -> RequestTransformer -> Template -> [(String, Value)] -> m (Either UpdateError a)
doReq f baseUrl apiToken reqTransformer template vars = case createRequest baseUrl apiToken reqTransformer template vars of
  Left invalidUrl -> pure $ Left invalidUrl
  Right request -> do
    result <- liftIO $ try (f request)
    pure $ mapLeft removeApiTokenFromUpdateError $ join $ mapLeft HttpError result
