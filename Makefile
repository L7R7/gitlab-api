check: format lint

lint:
	hlint  gitlab-api-servant/src gitlab-api-test/test gitlab-api-types/src

format:
	ormolu --mode check $(shell find gitlab-api-servant/src gitlab-api-test/test gitlab-api-types/src -type f -name "*.hs")

reformat:
	ormolu --mode inplace  $(shell find gitlab-api-servant/src gitlab-api-test/test gitlab-api-types/src -type f -name "*.hs")
