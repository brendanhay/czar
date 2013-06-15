all: build lint

build: .conf
	cabal-dev build && $(MAKE) bin/czar-server bin/czar-agent

install:
	cabal-dev install

clean:
	-rm -f .conf bin/czar-*

lint:
	hlint src

.conf:
	cabal-dev configure && touch .conf

bin/%:
	@mkdir -p bin && ln -fs ../dist/build/$*/$* $@
