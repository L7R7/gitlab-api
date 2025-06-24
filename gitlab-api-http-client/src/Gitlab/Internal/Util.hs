module Gitlab.Internal.Util
  ( RequestTransformer,
    createRequest,
    parseNextRequest,
    removeApiTokenFromUpdateError,
    setTimeout,
    addToken,
    addUserAgent,
  )
where

import Burrito
import Data.Bifunctor (bimap)
import Data.Either.Combinators (rightToMaybe)
import Data.Foldable (find)
import Data.Text.Encoding (encodeUtf8)
import Gitlab.Internal.Types
import Network.HTTP.Client.Conduit (requestFromURI, requestHeaders, responseTimeout, responseTimeoutMicro)
import Network.HTTP.Link.Parser (parseLinkHeaderBS)
import Network.HTTP.Link.Types (Link (..), LinkParam (Rel), href)
import Network.HTTP.Simple
import Network.HTTP.Types.Header (HeaderName)
import Network.URI (URI)

type RequestTransformer = Request -> Request

createRequest :: BaseUrl -> ApiToken -> UserAgent -> RequestTransformer -> Template -> [(String, Value)] -> Either UpdateError Request
createRequest baseUrl apiToken userAgent reqTransformer template vars =
  bimap
    ExceptionError
    (reqTransformer . setTimeout . addUserAgent userAgent . addToken apiToken)
    (parseRequest (show baseUrl <> "/" <> expand vars template))

setTimeout :: RequestTransformer
setTimeout request = request {responseTimeout = responseTimeoutMicro 5000000}

addToken :: ApiToken -> RequestTransformer
addToken (ApiToken apiToken) = setRequestHeader "PRIVATE-TOKEN" [encodeUtf8 apiToken]

addUserAgent :: UserAgent -> RequestTransformer
addUserAgent (UserAgent userAgent) = addRequestHeader "User-Agent" (encodeUtf8 userAgent)

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
