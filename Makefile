.PHONY: usage win

usage:
	@echo "Usage:"
	@echo "  make win"

win:
	stack build
	rm -rf win/
	mkdir win/
	cp $(shell stack exec which samplitude) win/
	cp $(shell stack exec which libsamplerate-0.dll) win/
