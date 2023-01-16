module Main (main) where

import API
import Data.Proxy
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS
import Servant.Client
import Spec
import System.Environment (lookupEnv)
import Test.Syd

main :: IO ()
main = sydTest $ modifyMaxSuccess (const 5) $ setupAroundAll ((,) <$> clientEnvSetupFunc <*> gitlabAPISetupFunc) spec

privateTokenSetupFunc :: SetupFunc String
privateTokenSetupFunc = SetupFunc $ \func -> do
  maybeToken <- lookupEnv "GITLAB_API_TOKEN"
  case maybeToken of
    Nothing -> expectationFailure "Expecting the API token for the requests in the environment variable \"GITLAB_API_TOKEN\""
    Just token -> func token

gitlabAPISetupFunc :: SetupFunc (GitlabAPI (AsClientT ClientM))
gitlabAPISetupFunc = client (Proxy @API) . Just <$> privateTokenSetupFunc

baseUrlSetupFunc :: SetupFunc String
baseUrlSetupFunc = SetupFunc $ \func -> do
  maybeBaseUrl <- lookupEnv "CI_SERVER_HOST"
  case maybeBaseUrl of
    Nothing -> expectationFailure "Expecting the base URL for the requests in the environment variable \"CI_SERVER_HOST\""
    Just baseUrl' -> func baseUrl'

clientEnvSetupFunc :: SetupFunc ClientEnv
clientEnvSetupFunc =
  baseUrlSetupFunc >>= \baseUrlString -> SetupFunc $ \func -> do
    manager' <- newManager tlsManagerSettings
    baseUrl' <- parseBaseUrl baseUrlString
    func $ mkClientEnv manager' baseUrl'
