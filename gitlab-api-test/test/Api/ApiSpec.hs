module Api.ApiSpec (spec) where

import Api.ApiDocs.OpenApi ()
import Data.OpenApi.Internal.Utils
import Data.Proxy
import Gitlab.API.API
import Servant.OpenApi (HasOpenApi (..))
import Servant.Server
import Test.Syd

spec :: TestDefM outers () ()
spec = describe "API structure" $ do
  it "documents the API in a golden file" $ do
    let apiStructure = layout (Proxy @API)
    pureGoldenTextFile "test_resources/api/api-structure.txt" apiStructure
  it "generates OpenAPI spec" $ do
    let apiStructure = encodePretty $ toOpenApi (Proxy @API)
    pureGoldenLazyByteStringFile "test_resources/api/openapi.json" apiStructure
