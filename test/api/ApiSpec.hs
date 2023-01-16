module ApiSpec (spec) where

import API
import Data.Proxy
import Servant.Server
import Test.Syd

spec :: TestDefM outers () ()
spec = describe "API structure" $
  it "documents the API in a golden file" $ do
    let apiStructure = layout (Proxy @API)
    pureGoldenTextFile "test/resources/api/api-structure.txt" apiStructure
