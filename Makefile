all: build lint

build: .conf
	cabal-dev build

install:
	cabal-dev install

clean:
	-rm -f .conf
	cabal-dev clean

lint:
	hlint src

.conf:
	cabal-dev configure && touch .conf
