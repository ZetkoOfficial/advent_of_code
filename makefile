run: build
	@bin/main.exe

build:
	@dune build --release
