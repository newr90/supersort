.PHONY: build dev clean

build:
	ghc ./suso_console.hs -outputdir _build -o suso_console -threaded -Wall -O2

dev:
	ghcid ./suso_console.hs

clean:
	rm -rf _build