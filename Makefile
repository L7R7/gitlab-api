check: format lint

lint:
	hlint  gitlab-api-http-client/src gitlab-api-http-client-mtl/src gitlab-api-servant/src gitlab-api-test/test gitlab-api-types/src

format:
	ormolu --mode check $(shell find gitlab-api-http-client/src gitlab-api-http-client-mtl/src gitlab-api-servant/src gitlab-api-test/test gitlab-api-types/src -type f -name "*.hs")

reformat:
	ormolu --mode inplace  $(shell find gitlab-api-http-client/src gitlab-api-http-client-mtl/src gitlab-api-servant/src gitlab-api-test/test gitlab-api-types/src -type f -name "*.hs")
