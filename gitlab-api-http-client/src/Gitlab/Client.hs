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
import Control.Exception.Base (SomeException, try)
import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson hiding (Value)
import Data.Bifunctor (bimap)
import Data.ByteString.Lazy qualified as L
import Data.Either.Combinators (mapLeft, rightToMaybe)
import Data.Foldable (find)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client.Conduit (requestFromURI, requestHeaders, responseTimeout, responseTimeoutMicro)
import Network.HTTP.Link.Parser (parseLinkHeaderBS)
import Network.HTTP.Link.Types (Link (..), LinkParam (Rel), href)
import Network.HTTP.Simple
import Network.HTTP.Types (Status, statusCode, statusIsSuccessful)
import Network.HTTP.Types.Header (HeaderName, hAccept)
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

fetchData :: (FromJSON a, MonadIO m) => BaseUrl -> ApiToken -> Template -> [(String, Value)] -> m (Either UpdateError a)
fetchData baseUrl apiToken = fetchData' baseUrl apiToken id

type RequestTransformer = Request -> Request

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
    Right request -> fetchDataPaginated' apiToken template request []

fetchDataPaginated' :: (FromJSON a, MonadIO m) => ApiToken -> Template -> Request -> [a] -> m (Either UpdateError [a])
fetchDataPaginated' apiToken template request acc = do
  result <- liftIO $ try $ do
    response <- httpJSONEither (setTimeout $ addToken apiToken request)
    let next = parseNextRequest response
    case mapLeft ConversionError $ getResponseBody response of
      Left err -> pure $ Left err
      Right as -> maybe (pure $ Right (as <> acc)) (\req -> fetchDataPaginated' apiToken template req (as <> acc)) next
  pure $ mapLeft removeApiTokenFromUpdateError $ join $ mapLeft HttpError result

headRequest :: (MonadIO m) => BaseUrl -> ApiToken -> RequestTransformer -> Template -> [(String, Value)] -> m (Either UpdateError Status)
headRequest baseUrl apiToken reqTransformer = doReq (fmap (Right . getResponseStatus) . httpNoBody) baseUrl apiToken (reqTransformer . setRequestMethod "HEAD")

doReq :: (MonadIO m) => (Request -> IO (Either UpdateError a)) -> BaseUrl -> ApiToken -> RequestTransformer -> Template -> [(String, Value)] -> m (Either UpdateError a)
doReq f baseUrl apiToken reqTransformer template vars = case createRequest baseUrl apiToken reqTransformer template vars of
  Left invalidUrl -> pure $ Left invalidUrl
  Right request -> do
    result <- liftIO $ try (f request)
    pure $ mapLeft removeApiTokenFromUpdateError $ join $ mapLeft HttpError result

createRequest :: BaseUrl -> ApiToken -> RequestTransformer -> Template -> [(String, Value)] -> Either UpdateError Request
createRequest baseUrl apiToken reqTransformer template vars =
  bimap
    ExceptionError
    (reqTransformer . setTimeout . addToken apiToken)
    (parseRequest (show baseUrl <> "/" <> expand vars template))

setTimeout :: RequestTransformer
setTimeout request = request {responseTimeout = responseTimeoutMicro 5000000}

addToken :: ApiToken -> RequestTransformer
addToken (ApiToken apiToken) = setRequestHeader "PRIVATE-TOKEN" [encodeUtf8 apiToken]

parseNextRequest :: Response a -> Maybe Request
parseNextRequest response = parseNextHeader response >>= rightToMaybe . requestFromURI

parseNextHeader :: Response a -> Maybe URI
parseNextHeader response = href <$> find isNextLink (getResponseHeader "link" response >>= concat . parseLinkHeaderBS)

isNextLink :: Link uri -> Bool
isNextLink (Link _ [(Rel, "next")]) = True
isNextLink _ = False

removeApiTokenFromUpdateError :: UpdateError -> UpdateError
removeApiTokenFromUpdateError (HttpError httpException) = HttpError (removeApiTokenFromHttpException httpException)
removeApiTokenFromUpdateError (ResponseWasNotSuccessful st x) = ResponseWasNotSuccessful st x
removeApiTokenFromUpdateError (ConversionError jsonException) = ConversionError (removeApiTokenFromJsonException jsonException)
removeApiTokenFromUpdateError (ConversionError' s) = ConversionError' s
removeApiTokenFromUpdateError (ExceptionError x) = ExceptionError x
removeApiTokenFromUpdateError (ParseUrlError x) = ParseUrlError x

removeApiTokenFromHttpException :: HttpException -> HttpException
removeApiTokenFromHttpException (HttpExceptionRequest request reason) = HttpExceptionRequest (removeApiTokenFromRequest request) reason
removeApiTokenFromHttpException e = e

privateToken :: HeaderName
privateToken = "PRIVATE-TOKEN"

removeApiTokenFromJsonException :: JSONException -> JSONException
removeApiTokenFromJsonException (JSONParseException request response parseError) = JSONParseException (removeApiTokenFromRequest request) response parseError
removeApiTokenFromJsonException (JSONConversionException request response s) = JSONConversionException (removeApiTokenFromRequest request) response s

removeApiTokenFromRequest :: RequestTransformer
removeApiTokenFromRequest request = request {requestHeaders = newHeaders}
  where
    newHeaders = replaceToken <$> requestHeaders request
    replaceToken header@(name, _)
      | name == privateToken = (name, "xxxxx")
      | otherwise = header
