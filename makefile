run-all: build
	@bin/main.exe

run: build
	@bin/main.exe $(day)

build:
	@dune build