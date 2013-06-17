all: build lint

build: .conf czar/Czar/Protocol.hs
	cabal-dev build && $(MAKE) bin/czar-server bin/czar-agent

install:
	cabal-dev install

clean:
	-rm -rf .conf bin/czar-* src/Czar/Protocol*

lint:
	hlint czar-agent czar-server

.conf:
	cabal-dev configure && touch .conf

bin/%:
	@mkdir -p bin && ln -fs ../dist/build/$*/$* $@

czar/Czar/Protocol.hs: lib/czar.proto
	hprotoc -I lib -p Czar -d czar -v $<
