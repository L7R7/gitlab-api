# gitlab-api

## Goals

* Provide
  * a servant definition of the Gitlab API
  * JSON encodings for all the dataypes involved
* Use tests to make sure the bindings (especially the JSON decoders) are as correct as possible
* Based on the servant definition, derive other useful artifacts:
  * a textual representation of the API
  * OpenAPI spec

## possible future features

* Add more endpoints
* Make the enpdoints aware of query parameters
  * pagination
  * sorting
  * before/after
* Improve the OpenAPI spec
* Provide some helpers to make the use of servant client bindings more pleasing
  * especially for paginated resources

## prior art

* [https://hackage.haskell.org/package/servant-github](https://hackage.haskell.org/package/servant-github)
  * This is pretty close to the idea I had initially: Model the API as a servant type and then provide handy client functions
  * I have to find out how all the type level stuff works in there
* [https://hackage.haskell.org/package/gitlab-haskell](https://hackage.haskell.org/package/gitlab-haskell)
  * last time I checked, it didn't cover the parts of the API I'm interested in

## endpoint candidates

* :white_check_mark: `/api/v4/groups?all_available=true`
  * :white_check_mark: `/api/v4/groups/{groupId}/projects`
    * :white_check_mark: `/api/v4/groups/{groupId}/projects?include_subgroups=true&archived=false&with_shared=false`
    * :question: `/api/v4/groups/{groupId}/projects?simple=true`
    * :question: `/api/v4/groups/{groupId}/runners?status=online&type=group_type`
* :question: `/api/v4/projects/{projectId}`
  * :question: `/api/v4/projects/{projectId}/hooks`
  * :question: `/api/v4/projects/{projectId}/jobs{?scope%5B%5D*}`
  * :question: `/api/v4/projects/{projectId}/jobs?scope=success&per_page=100`
  * :question: `/api/v4/projects/{projectId}/merge_requests?state=opened`
  * :question: `/api/v4/projects/{projectId}?only_allow_merge_if_all_discussions_are_resolved=true`
  * :question: `/api/v4/projects/{projectId}?only_allow_merge_if_pipeline_succeeds=true`
  * :question: `/api/v4/projects/{projectId}/pipeline_schedules`
  * :question: `/api/v4/projects/{projectId}/pipelines/{pipelineId}`
    * :question: `/api/v4/projects/{projectId}/pipelines?ref={ref}&per_page=1`
    * :question: `/api/v4/projects/{projectId}/pipelines?ref={ref}&status={status}&updated_after=2021-01-06T00:00:00Z&source=push`
    * :question: `/api/v4/projects/{projectId}/pipelines?ref={ref}&status={status}&updated_after=2022-01-01T00:00:00Z&updated_before=2023-01-01T00:00:00Z&source=push`
  * :question: `/api/v4/projects/{projectId}?remove_source_branch_after_merge=true`
  * :question: `/api/v4/projects/{projectId}/repository/branches`
  * :question: `/api/v4/projects/{projectId}/repository/files/.gitlab-ci.yml?ref={ref}`
  * :question: `/api/v4/projects/{projectId}/runners?type=project_type`
* :question: `/api/v4/runners/{runnerId}/jobs?status=running`
* :question: `/api/v4/users`
  * :question: `/api/v4/users/{userId}/projects?archived=false`
