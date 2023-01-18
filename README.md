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
