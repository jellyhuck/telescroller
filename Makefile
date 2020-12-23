ELMC    := elm

TARGETS:= main
OUTS   := $(addsuffix .js, $(TARGETS) )

.PHONY: all clean docs

all: $(TARGETS)

clean:
	rm -f $(OUTS)

main: src/Main.elm src/index.html 
	$(ELMC) make src/Main.elm --output build/$@.js
	cp src/index.html build/index.html

docs: src/Main.elm src/index.html
	$(ELMC) make src/Main.elm --optimize --output docs/main.js
	cp src/index.html docs/index.html
