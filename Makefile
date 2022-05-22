.PHONY: clean

main:
	ocamlopt -o main main.ml

clean:
	rm *.cmi *.cmx *.o main

