module Main (main) where

import API
import Data.Proxy
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS
import Servant.Client
import qualified Unit.Spec
import qualified Api.Spec
import System.Environment (lookupEnv)
import Test.Syd

main :: IO ()
main = sydTest $ do
  beforeAll ((,) <$> clientEnvSetup <*> gitlabAPIClient) Api.Spec.spec
  Unit.Spec.spec

privateTokenSetup :: IO String
privateTokenSetup = do
  maybeToken <- lookupEnv "GITLAB_API_TOKEN"
  case maybeToken of
    Nothing -> expectationFailure "Expecting the API token for the requests in the environment variable \"GITLAB_API_TOKEN\""
    Just token -> pure token

gitlabAPIClient :: IO (GitlabAPI (AsClientT ClientM))
gitlabAPIClient = client (Proxy @API) . Just <$> privateTokenSetup

baseUrlSetup :: IO String
baseUrlSetup = do
  maybeBaseUrl <- lookupEnv "CI_SERVER_HOST"
  case maybeBaseUrl of
    Nothing -> expectationFailure "Expecting the base URL for the requests in the environment variable \"CI_SERVER_HOST\""
    Just baseUrl' -> pure baseUrl'

clientEnvSetup :: IO ClientEnv
clientEnvSetup =
  baseUrlSetup >>= \baseUrlString -> do
    manager' <- newManager tlsManagerSettings
    baseUrl' <- parseBaseUrl baseUrlString
    pure $ mkClientEnv manager' baseUrl'
