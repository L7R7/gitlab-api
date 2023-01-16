{-# OPTIONS_GHC -Wno-orphans #-}

module ProjectSpec (spec) where

import API
import Data.Either
import Data.Either.Extra
import qualified Job
import Lib
import Network.HTTP.Types
import Project
import Servant.Client
import Test.Syd
import Test.Syd.Validity

spec :: TestDef ((ClientEnv, GitlabAPI (AsClientT ClientM)) : otherOuters) ()
spec = beforeAllWith (pure . fmap project) $ describe "project" $ do
  itWithOuter "all projects" $ \(clientEnv, ProjectAPI allProjects _) -> do
    res <- runClientM allProjects clientEnv
    res `shouldSatisfy` isRight
    shouldBeValid $ eitherToMaybe res
  itWithOuter "single project" $ \(clientEnv, ProjectAPI _ singleProjectApi) -> do
    let pId = Id @Project 720 -- todo: make this more robust by using different projects
    let (SingleProjectAPI singleProject _) = singleProjectApi pId
    res <- runClientM singleProject clientEnv
    shouldBeValid res
    (projectId <$> res) `shouldSatisfy` elem pId
  itWithOuter "single job" $ \(clientEnv, ProjectAPI _ singleProjectApi) -> do
    let pId = Id @Project 720 -- todo: make this more robust by using different projects
    let jId = Id @Job.Job 4028634
    let (SingleProjectAPI _ (JobAPI singleJob)) = singleProjectApi pId
    res <- runClientM (singleJob jId) clientEnv
    shouldBeValid res
    (Job.jobId <$> res) `shouldSatisfy` all (jId ==)

-- todo: move me to a better place and consider sharing the instance with the one from Utils.hs in unit-test
instance GenValid (Id a)

instance Validity ClientError where
  validate (FailureResponse _ response) | responseStatusCode response == status404 = valid
  validate clientError = invalid $ show clientError
