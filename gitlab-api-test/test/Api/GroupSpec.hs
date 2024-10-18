{-# OPTIONS_GHC -Wno-orphans #-}

module Api.GroupSpec (spec) where

import Data.Either
import Data.Either.Extra
import Data.Foldable (for_)
import Gitlab.API.API
import Gitlab.API.Group
import Gitlab.Group
import Gitlab.Lib
import Network.HTTP.Types
import Servant.Client
import Test.Syd
import Test.Syd.Validity

groupsToCheck :: [Id Group]
groupsToCheck = [Id 66, Id 15, Id 288, Id 254]

spec :: TestDef ((ClientEnv, GitlabAPI (AsClientT ClientM)) : otherOuters) ()
spec = beforeAllWith (pure . fmap group) $ describe "group" $ do
  itWithOuter "all groups" $ \(clientEnv, GroupAPI allGroups _) -> do
    res <- runClientM (allGroups False) clientEnv
    res `shouldSatisfy` isRight
    shouldBeValid $ eitherToMaybe res
  itWithOuter "all groups (all_available)" $ \(clientEnv, GroupAPI allGroups _) -> do
    res <- runClientM (allGroups True) clientEnv
    res `shouldSatisfy` isRight
    shouldBeValid $ eitherToMaybe res
  describe "single group" $ do
    describe "get group" $
      for_ groupsToCheck $ \gId -> itWithOuter (show gId) $ \(clientEnv, GroupAPI _ singleGroupApi) -> do
        let (SingleGroupAPI singleGroup _ _) = singleGroupApi gId
        res <- runClientM singleGroup clientEnv
        shouldBeValid res
        (groupId <$> res) `shouldSatisfy` elem gId
    describe "get group projects" $ do
      for_ groupsToCheck $ \gId -> itWithOuter (show gId) $ \(clientEnv, GroupAPI _ singleGroupApi) -> do
        let (SingleGroupAPI _ groupProjects _) = singleGroupApi gId
        res <- runClientM (groupProjects False False False) clientEnv
        shouldBeValid res
      -- (groupId <$> res) `shouldSatisfy` elem gId todo: revisit when there's a group link in the project?
      describe "with subgroups" $
        for_ groupsToCheck $ \gId -> itWithOuter (show gId) $ \(clientEnv, GroupAPI _ singleGroupApi) -> do
          let (SingleGroupAPI _ groupProjects _) = singleGroupApi gId
          res <- runClientM (groupProjects True False False) clientEnv
          shouldBeValid res
      describe "with shared" $
        for_ groupsToCheck $ \gId -> itWithOuter (show gId) $ \(clientEnv, GroupAPI _ singleGroupApi) -> do
          let (SingleGroupAPI _ groupProjects _) = singleGroupApi gId
          res <- runClientM (groupProjects False True False) clientEnv
          shouldBeValid res
      describe "with archived" $
        for_ groupsToCheck $ \gId -> itWithOuter (show gId) $ \(clientEnv, GroupAPI _ singleGroupApi) -> do
          let (SingleGroupAPI _ groupProjects _) = singleGroupApi gId
          res <- runClientM (groupProjects False False True) clientEnv
          shouldBeValid res
    describe "get group merge requests" $ do
      for_ groupsToCheck $ \gId -> itWithOuter (show gId) $ \(clientEnv, GroupAPI _ singleGroupApi) -> do
        let (SingleGroupAPI _ _ groupMergeRequests) = singleGroupApi gId
        res <- runClientM groupMergeRequests clientEnv
        shouldBeValid res

instance Validity ClientError where
  validate (FailureResponse _ response) | responseStatusCode response == status404 = valid
  validate clientError = invalid $ show clientError
