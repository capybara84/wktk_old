PROGRAM = wktk

all : byte

test : byte
	ocamlrun $(PROGRAM).byte -t

clean:
	ocamlbuild -clean

native:
	ocamlbuild $(PROGRAM).native

byte:
	ocamlbuild $(PROGRAM).byte


