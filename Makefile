.DEFAULT_GOAL := compile

compile: build

build:
	corebuild erlangport.cma


install:
	ocamlfind install node META _build/*

remove:
	ocamlfind remove transit

clean:
	corebuild -clean
