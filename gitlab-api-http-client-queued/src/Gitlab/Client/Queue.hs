module Gitlab.Client.Queue
  ( BaseUrl (..),
    ApiToken (..),
    UserAgent (..),
    UpdateError (..),
    QueueConfig (..),
    ProcessResult (..),
    fetchDataQueued,
  )
where

import Burrito
import Control.Concurrent.STM.TBMQueue
import Control.Monad (join, replicateM)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Unlift
import Data.Aeson hiding (Value)
import Data.Either.Combinators (mapLeft)
import Data.Foldable (traverse_)
import Data.List.NonEmpty
import Data.Text
import Gitlab.Internal.Types
import Gitlab.Internal.Util
import Network.HTTP.Simple
import System.Console.Concurrent
import UnliftIO.Async
import UnliftIO.Exception (try)
import UnliftIO.STM (atomically)

data QueueConfig = QueueConfig
  { parallelism :: Int,
    bufferSize :: Int
  }

data ProcessResult a = Empty | Result a | PrintLines (NonEmpty Text) | PrintLinesWithResult (NonEmpty Text) a

fetchDataQueued ::
  forall a b m.
  (FromJSON a, MonadUnliftIO m, MonadMask m) =>
  BaseUrl ->
  ApiToken ->
  UserAgent ->
  Template ->
  [(String, Value)] ->
  QueueConfig ->
  (a -> m (Either UpdateError (ProcessResult b))) ->
  m (Either UpdateError [b])
fetchDataQueued baseUrl apiToken userAgent template vars (QueueConfig parallelism bufferSize) processFunction = withConcurrentOutput $ do
  queue <- liftIO $ newTBMQueueIO bufferSize
  let processFromQueue = processFromQueue' []
      processFromQueue' acc = do
        res <- atomically (tryReadTBMQueue queue)
        case res of
          Nothing -> pure $ Right acc
          Just Nothing -> processFromQueue' acc
          Just (Just a) -> do
            res' <- processFunction a
            case res' of
              Left err -> pure $ Left err
              Right Empty ->
                processFromQueue' acc
              Right (Result b) ->
                processFromQueue' (b : acc)
              Right (PrintLines linesToPrint) -> do
                traverse_ (\line -> liftIO $ outputConcurrent $ line <> "\n") linesToPrint
                processFromQueue' acc
              Right (PrintLinesWithResult linesToPrint b) -> do
                traverse_ (\line -> liftIO $ outputConcurrent $ line <> "\n") linesToPrint
                processFromQueue' (b : acc)
      enqueue request = do
        res <- fetchDataQueued' @a apiToken request queue
        _ <- atomically (closeTBMQueue queue)
        pure res
  case createRequest baseUrl apiToken userAgent id template vars of
    Left invalidUrl -> pure $ Left invalidUrl
    Right request ->
      runConcurrently
        ( (\enqueueing processors -> fmap join $ enqueueing *> sequence processors)
            <$> Concurrently (enqueue request)
            <*> replicateM parallelism (Concurrently processFromQueue)
        )

fetchDataQueued' :: forall a m. (FromJSON a, MonadUnliftIO m) => ApiToken -> Request -> TBMQueue a -> m (Either UpdateError ())
fetchDataQueued' apiToken request queue = do
  result <- try $ do
    response <- httpJSONEither @_ @[a] (setTimeout $ addToken apiToken request)
    let next = parseNextRequest response
    case mapLeft ConversionError $ getResponseBody response of
      Left err -> pure $ Left err
      Right as -> do
        traverse_ (atomically . writeTBMQueue queue) as
        maybe (pure (Right ())) (\req -> fetchDataQueued' apiToken req queue) next
  pure $ mapLeft removeApiTokenFromUpdateError $ join $ mapLeft HttpError result
