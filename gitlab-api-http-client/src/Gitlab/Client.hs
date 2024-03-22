module Gitlab.Client
  ( BaseUrl (..),
    ApiToken (..),
    UpdateError (..),
    fetchData,
    fetchDataPaginated,
    headRequest,
  )
where

import Burrito
import Control.Exception.Base (SomeException, try)
import Control.Lens (Lens', Prism', Traversal', filtered, lens, prism', set, _1, _2)
import Control.Monad (join)
import Data.Aeson hiding (Value)
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.Either.Combinators (mapLeft, rightToMaybe)
import Data.Foldable (find)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client.Conduit (HttpExceptionContent, requestFromURI, requestHeaders, responseTimeout, responseTimeoutMicro)
import Network.HTTP.Link.Parser (parseLinkHeaderBS)
import Network.HTTP.Link.Types (Link (..), LinkParam (Rel), href)
import Network.HTTP.Simple
import Network.HTTP.Types (Status)
import Network.HTTP.Types.Header (HeaderName)
import Network.URI (URI)

newtype BaseUrl = BaseUrl URI deriving newtype (Show)

newtype ApiToken = ApiToken Text deriving newtype (FromJSON, Show)

data UpdateError
  = HttpError HttpException
  | ExceptionError SomeException
  | ConversionError JSONException
  | ParseUrlError Text
  deriving stock (Show)

fetchData :: (FromJSON a) => BaseUrl -> ApiToken -> Template -> [(String, Value)] -> IO (Either UpdateError a)
fetchData baseUrl apiToken = fetchData' baseUrl apiToken id

type RequestTransformer = Request -> Request

fetchData' :: (FromJSON a) => BaseUrl -> ApiToken -> RequestTransformer -> Template -> [(String, Value)] -> IO (Either UpdateError a)
fetchData' = doReq (fmap (mapLeft ConversionError . getResponseBody) . httpJSONEither)

fetchDataPaginated :: (FromJSON a) => ApiToken -> BaseUrl -> Template -> [(String, Value)] -> IO (Either UpdateError [a])
fetchDataPaginated apiToken baseUrl template vars =
  case createRequest baseUrl apiToken id template vars of
    Left invalidUrl -> pure $ Left invalidUrl
    Right request -> fetchDataPaginated' apiToken template request []

fetchDataPaginated' :: (FromJSON a) => ApiToken -> Template -> Request -> [a] -> IO (Either UpdateError [a])
fetchDataPaginated' apiToken template request acc = do
  result <- try $ do
    response <- httpJSONEither (setTimeout $ addToken apiToken request)
    let next = parseNextRequest response
    case mapLeft ConversionError $ getResponseBody response of
      Left err -> pure $ Left err
      Right as -> maybe (pure $ Right (as <> acc)) (\req -> fetchDataPaginated' apiToken template req (as <> acc)) next
  pure $ mapLeft removeApiTokenFromUpdateError $ join $ mapLeft HttpError result

headRequest :: BaseUrl -> ApiToken -> RequestTransformer -> Template -> [(String, Value)] -> IO (Either UpdateError Status)
headRequest baseUrl apiToken reqTransformer = doReq (fmap (Right . getResponseStatus) . httpNoBody) baseUrl apiToken (reqTransformer . setRequestMethod "HEAD")

doReq :: (Request -> IO (Either UpdateError a)) -> BaseUrl -> ApiToken -> RequestTransformer -> Template -> [(String, Value)] -> IO (Either UpdateError a)
doReq f baseUrl apiToken reqTransformer template vars = case createRequest baseUrl apiToken reqTransformer template vars of
  Left invalidUrl -> pure $ Left invalidUrl
  Right request -> do
    result <- try (f request)
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
removeApiTokenFromUpdateError (ConversionError jsonException) = ConversionError (removeApiTokenFromJsonException jsonException)
removeApiTokenFromUpdateError (ExceptionError x) = ExceptionError x
removeApiTokenFromUpdateError (ParseUrlError x) = ParseUrlError x

removeApiTokenFromHttpException :: HttpException -> HttpException
removeApiTokenFromHttpException = set (reqPrism . _1 . headers . tokenHeader) "xxxxx"

reqPrism :: Prism' HttpException (Request, HttpExceptionContent)
reqPrism = prism' (uncurry HttpExceptionRequest) extract
  where
    extract (HttpExceptionRequest request reason) = Just (request, reason)
    extract _ = Nothing

tokenHeader :: Traversal' RequestHeaders ByteString
tokenHeader = traverse . filtered (\h -> fst h == privateToken) . _2

privateToken :: HeaderName
privateToken = "PRIVATE-TOKEN"

headers :: Lens' Request RequestHeaders
headers = lens getter setter
  where
    getter = requestHeaders
    setter r h = r {requestHeaders = h}

removeApiTokenFromJsonException :: JSONException -> JSONException
removeApiTokenFromJsonException (JSONParseException request response parseError) = JSONParseException (removeApiTokenFromRequest request) response parseError
removeApiTokenFromJsonException (JSONConversionException request response s) = JSONConversionException (removeApiTokenFromRequest request) response s

removeApiTokenFromRequest :: RequestTransformer
removeApiTokenFromRequest = set (headers . tokenHeader) "xxxxx"
