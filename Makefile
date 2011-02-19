
all:	doc tool

tool:
	bin/referl -build tool

doc:
	bin/referl -build doc

clean:
	bin/referl -build clean

