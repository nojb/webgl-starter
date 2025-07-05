serve: build
	uv run --managed-python python -m http.server

build:
	opam exec -- dune build --profile release
