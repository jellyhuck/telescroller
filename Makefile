ELMC    := elm

TARGETS:= main
OUTS   := $(addsuffix .js, $(TARGETS) )

.PHONY: all clean

all: $(TARGETS)

clean:
	rm -f $(OUTS)

main: src/Main.elm src/index.html 
	$(ELMC) make src/Main.elm --output $@.js
