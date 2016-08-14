.PHONY: build clean

BLUE=\n\033[0;34m

build:
	@echo "$(BLUE) @@@ (build) Building distributable..."
	raco make main.rkt
	raco exe -o pinboard-pocket main.rkt
	mkdir pinboard2pocket
	raco distribute pinboard2pocket pinboard-pocket

clean:
	@echo "$(BLUE) @@@ (clean) Cleaning dist..."
	rm -rf pinboard2pocket
