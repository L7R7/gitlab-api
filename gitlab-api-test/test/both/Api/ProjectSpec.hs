{-# OPTIONS_GHC -Wno-orphans #-}

module Api.ProjectSpec (spec) where

import Data.Either
import Data.Either.Extra
import Data.Foldable (for_)
import Gitlab.API.API
import Gitlab.API.Project
import Gitlab.Job
import Gitlab.Lib
import Gitlab.Project
import Network.HTTP.Types
import Servant.Client
import Test.Syd
import Test.Syd.Validity
import Utils ()

spec :: TestDef ((ClientEnv, GitlabAPI (AsClientT ClientM)) : otherOuters) ()
spec = beforeAllWith (pure . fmap project) $ describe "project" $ do
  itWithOuter "all projects" $ \(clientEnv, ProjectAPI allProjects _) -> do
    res <- runClientM allProjects clientEnv
    res `shouldSatisfy` isRight
    shouldBeValid $ eitherToMaybe res
  describe "single project" $
    for_ [Id @Project 720, Id 795, Id 3015, Id 2679] $ \pId -> itWithOuter (show pId) $ \(clientEnv, ProjectAPI _ singleProjectApi) -> do
      let (SingleProjectAPI singleProject _ _) = singleProjectApi pId
      res <- runClientM singleProject clientEnv
      shouldBeValid res
      (projectId <$> res) `shouldSatisfy` elem pId
  describe "single job" $
    -- todo: Id 4117230. a skipped job has no started_at
    for_ [Id @Job 4028634, Id 4117227, Id 4114940] $ \jId -> itWithOuter (show jId) $ \(clientEnv, ProjectAPI _ singleProjectApi) -> do
      let pId = Id @Project 720
      let (SingleProjectAPI _ (JobAPI singleJob) _) = singleProjectApi pId
      res <- runClientM (singleJob jId) clientEnv
      shouldBeValid res
      (jobId <$> res) `shouldSatisfy` all (jId ==)

  describe "merge requests" $ do
    describe "list all" $ for_ [Id @Project 720, Id 795, Id 3015, Id 2679] $ \pId -> itWithOuter (show pId) $ \(clientEnv, ProjectAPI _ singleProjectApi) -> do
      let (SingleProjectAPI _ _ (MergeRequestAPI mergeRequests)) = singleProjectApi pId
      res <- runClientM (mergeRequests Nothing Nothing) clientEnv
      res `shouldSatisfy` isRight
      shouldBeValid $ eitherToMaybe res

    describe "list from author" $
      for_ [Id 66, Id 144] $ \authorId ->
        for_ [Id @Project 720, Id 795, Id 3015, Id 2679] $ \pId -> itWithOuter ("Author(" <> show authorId <> ") - " <> show pId) $
          \(clientEnv, ProjectAPI _ singleProjectApi) -> do
            let (SingleProjectAPI _ _ (MergeRequestAPI mergeRequests)) = singleProjectApi pId
            res <- runClientM (mergeRequests Nothing Nothing) clientEnv
            res `shouldSatisfy` isRight
            shouldBeValid $ eitherToMaybe res

instance Validity ClientError where
  validate (FailureResponse _ response) | responseStatusCode response == status404 = valid
  validate clientError = invalid $ show clientError
