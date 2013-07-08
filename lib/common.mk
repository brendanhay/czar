DEPS ?=

SANDBOX := -s ../cabal-dev

APP := $(lastword $(subst /, ,$(CURDIR)))
BIN := dist/build/$(APP)/$(APP)

default: all

all: build

build: .conf $(DEPS)
	cabal-dev build $(SANDBOX) && $(MAKE) ../bin/$(APP)

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

../bin/$(APP): ../bin
	@if [ -e $(BIN) ]; then ln -fs ../$(APP)/$(BIN) $@; fi

../bin:
	-mkdir -p ../bin
