all: build lint

build: .conf src/Czar/Protocol.hs
	cabal-dev build && $(MAKE) bin/czar-server bin/czar-agent

install:
	cabal-dev install

clean:
	-rm -rf .conf bin/czar-* src/Czar/Protocol*

lint:
	hlint src

.conf:
	cabal-dev configure && touch .conf

bin/%:
	@mkdir -p bin && ln -fs ../dist/build/$*/$* $@

src/Czar/Protocol.hs: lib/czar.proto
	hprotoc -I lib -p Czar -d src -v $<
