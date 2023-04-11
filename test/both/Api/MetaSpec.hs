module Api.MetaSpec (spec) where

import API
import Data.Either
import Data.Either.Extra
import Meta
import Servant.Client
import Test.Syd
import Test.Syd.Validity

spec :: TestDef ((ClientEnv, GitlabAPI (AsClientT ClientM)) : otherOuters) ()
spec = beforeAllWith (pure . fmap meta) $ describe "meta" $ do
  itWithOuter "version" $ \(clientEnv, MetaAPI version _) -> do
    res <- runClientM version clientEnv
    res `shouldSatisfy` isRight
    shouldBeValid $ eitherToMaybe res
  itWithOuter "metadata" $ \(clientEnv, MetaAPI _ metadata) -> do
    res <- runClientM metadata clientEnv
    res `shouldSatisfy` isRight
    shouldBeValid $ eitherToMaybe res
  itWithOuter "version and metadata return the same content" $ \(clientEnv, MetaAPI version metadata) -> do
    res <- runClientM version clientEnv
    res' <- runClientM metadata clientEnv
    case (res, res') of
      (Right (Version v r), Right (Metadata v' r')) -> do
        v `shouldBe` v'
        r `shouldBe` r'
      x -> expectationFailure $ "expected two Rights, got " <> show x
