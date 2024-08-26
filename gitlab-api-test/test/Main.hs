{-# LANGUAGE TupleSections #-}

module Main (main) where

import Api.Spec qualified
import Data.Proxy
import Gitlab.API.API
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS
import Servant.Client
import System.Environment (lookupEnv)
import Test.Syd
import Unit.Spec qualified

main :: IO ()
main = do
  maybeApiSpecSetup <- apiSpecSetup
  sydTest $ do
    Unit.Spec.spec
    case maybeApiSpecSetup of
      Nothing -> pending "API Spec skipped because API Token and/or Base URL are missing"
      Just apiSpecSetup' -> beforeAll apiSpecSetup' Api.Spec.spec

apiSpecSetup :: IO (Maybe (IO (ClientEnv, GitlabAPI (AsClientT ClientM))))
apiSpecSetup = do
  maybePrivateToken <- lookupEnv "GITLAB_API_TOKEN"
  maybeBaseUrl <- lookupEnv "CI_SERVER_HOST"
  pure $ do
    token <- maybePrivateToken
    baseUrl <- maybeBaseUrl
    pure $ do
      let apiClient = client (Proxy @API) (Just token)
      (,apiClient) <$> clientEnvSetup baseUrl

clientEnvSetup :: String -> IO ClientEnv
clientEnvSetup baseUrlString = do
  manager' <- newManager tlsManagerSettings
  baseUrl' <- parseBaseUrl baseUrlString
  pure $ mkClientEnv manager' baseUrl'
