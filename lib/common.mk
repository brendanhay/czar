DEPS ?=
SANDBOX := -s ../cabal-dev

default: all

all: build

build: .conf $(DEPS)
	cabal-dev build $(SANDBOX)

install:
	cabal-dev install $(SANDBOX) -j \
	 --disable-documentation \
	 --disable-library-coverage

lint:
	hlint src

clean:
	-rm -rf dist $(DEPS) .conf

.conf:
	cabal-dev configure $(SANDBOX) && touch .conf
