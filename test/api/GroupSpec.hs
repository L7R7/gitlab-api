{-# OPTIONS_GHC -Wno-orphans #-}

module GroupSpec (spec) where

import API
import Data.Either
import Data.Either.Extra
import Data.Foldable (for_)
import Group
import Lib
import Network.HTTP.Types
import Servant.Client
import Test.Syd
import Test.Syd.Validity

spec :: TestDef ((ClientEnv, GitlabAPI (AsClientT ClientM)) : otherOuters) ()
spec = beforeAllWith (pure . fmap group) $ describe "group" $ do
  itWithOuter "all groups" $ \(clientEnv, GroupAPI allGroups _) -> do
    res <- runClientM allGroups clientEnv
    res `shouldSatisfy` isRight
    shouldBeValid $ eitherToMaybe res
  describe "single group" $
    for_ [Id @Group 66, Id 15, Id 288] $ \gId -> itWithOuter (show gId) $ \(clientEnv, GroupAPI _ singleGroupApi) -> do
      let (SingleGroupAPI singleGroup) = singleGroupApi gId
      res <- runClientM singleGroup clientEnv
      shouldBeValid res
      (groupId <$> res) `shouldSatisfy` elem gId

-- todo: move me to a better place and consider sharing the instance with the one from Utils.hs in unit-test
instance GenValid (Id a)

instance Validity ClientError where
  validate (FailureResponse _ response) | responseStatusCode response == status404 = valid
  validate clientError = invalid $ show clientError
