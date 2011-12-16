
.PHONY: all clean

SOURCES=$(wildcard *.dot)
TARGETS=$(SOURCES:.dot=.svg) $(SOURCES:.dot=.png)

all: $(TARGETS)

%.svg: %.dot
	dot -Tsvg -o$*.svg $*.dot

%.png: %.dot
	dot -Tpng -o$*.png $*.dot

clean:
	-rm $(TARGETS)

