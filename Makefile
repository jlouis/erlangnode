.DEFAULT_GOAL := compile

compile: build

build:
	corebuild erlangport.cma
	corebuild erlangport.cmxa
	corebuild main.native

install:
	ocamlfind install node META _build/*

remove:
	ocamlfind remove transit

clean:
	corebuild -clean
