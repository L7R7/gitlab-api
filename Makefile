check: format lint

lint:
	hlint src/ test/

format:
	ormolu --mode check $(shell find src/ test/ -type f -name "*.hs")

reformat:
	ormolu --mode inplace $(shell find src/ test/ -type f -name "*.hs")
