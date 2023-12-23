run-all: build
	@bin/main.exe

run: build
	@bin/main.exe $(day)
	@cat out/day_$(day)*

build:
	@dune build