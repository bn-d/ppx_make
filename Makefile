DUNE=dune

clean:
	$(DUNE) clean $(DUNE_ARGS)

build:
	$(DUNE) build $(DUNE_ARGS)

test:
	$(DUNE) runtest $(DUNE_ARGS)

format:
	$(DUNE) build @fmt --auto-promote

.PHONY: clean build test format
.DEFAULT: build



