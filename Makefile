c = corebuild -pkg yojson

type:
	$(c) type.native; cp type.native test
enumeration:
	$(c) enumeration.native; cp enumeration.native test
compression:
	$(c) compression.native; cp compression.native test
task:
	$(c) task.native; cp task.native test
list:
	$(c) list_synthesizer.native; cp list_synthesizer.native test
clean:
	rm -rf _build test
run:
	time ./test
