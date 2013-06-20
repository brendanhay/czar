all: build lint

build: .conf czar/Czar/Internal/Protocol.hs
	cabal-dev build && \
	 $(MAKE) bin/czar-server bin/czar-agent bin/czar-graphite

install:
	cabal-dev install

clean:
	-rm -rf .conf bin/czar-* czar/Czar/Internal/Protocol*

lint:
	hlint czar-agent czar-server

.conf:
	cabal-dev configure && touch .conf

bin/%:
	@mkdir -p bin && ln -fs ../dist/build/$*/$* $@

czar/Czar/Internal/Protocol.hs: lib/czar.proto
	hprotoc -I lib -p Czar.Internal -d czar -v $<
