module Gitlab.Hook (Hook (..)) where

import Autodocodec
import Data.Aeson (FromJSON)
import Data.Validity.Path ()
import Gitlab.Lib (Id, Url)

data Hook = Hook
  { hookId :: Id Hook,
    url :: Url Hook, -- todo dedicated target url phantom?
    pushEvents :: Bool,
    tagPushEvents :: Bool,
    mergeRequestEvents :: Bool,
    repositoryUpdateEvents :: Bool,
    issuesEvents :: Bool,
    confidentialIssuesEvents :: Bool,
    noteEvents :: Bool,
    confidentialNoteEvents :: Bool,
    pipelineEvents :: Bool,
    wikiPageEvents :: Bool,
    deploymentEvents :: Bool,
    jobEvents :: Bool,
    releasesEvents :: Bool
  }
  deriving (FromJSON) via (Autodocodec Hook)

instance HasCodec Hook where
  codec =
    object "Hook" $
      Hook
        <$> requiredField' "id"
          .= hookId
        <*> requiredField' "url"
          .= url
        <*> optionalFieldWithDefaultWith' "push_events" boolCodec False
          .= pushEvents
        <*> requiredField' "tag_push_events"
          .= tagPushEvents
        <*> requiredField' "merge_requests_events"
          .= mergeRequestEvents
        <*> requiredField' "repository_update_events"
          .= repositoryUpdateEvents
        <*> requiredField' "issues_events"
          .= issuesEvents
        <*> requiredField' "confidential_issues_events"
          .= confidentialIssuesEvents
        <*> requiredField' "note_events"
          .= noteEvents
        <*> (or <$> (optionalFieldOrNullWith' "confidential_note_events" boolCodec .= (pure . confidentialNoteEvents)))
        <*> requiredField' "pipeline_events"
          .= pipelineEvents
        <*> requiredField' "wiki_page_events"
          .= wikiPageEvents
        <*> requiredField' "deployment_events"
          .= deploymentEvents
        <*> requiredField' "job_events"
          .= jobEvents
        <*> requiredField' "releases_events"
          .= releasesEvents
