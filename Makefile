all:
	opam exec -- dune exec bpe-huffman

file:
	opam exec -- dune exec bpe-huffman .\test_file

utop:
	opam exec -- dune utop

build: 
	opam exec -- dune build

format:
	opam exec -- dune fmt