DEPS := vendor/ekg \
 vendor/options \
 vendor/pool \
 vendor/bouquet \
 czar/src/Czar/Internal/Protocol.hs

BINS := bin/server \
 bin/agent \
 bin/graphite \
 bin/pagerduty \
 bin/health

all: build

build: .conf $(BINS)
	cabal-dev build

install: $(DEPS)
	cabal-meta install --dev -j \
	 --disable-documentation \
	 --disable-library-coverage

clean:
	-rm -rf .conf bin dist vendor .shelly

lint:
	hlint czar-* system-info/src

.conf:
	cabal-dev configure && touch .conf

vendor/pool:
	git clone git@github.com:bos/pool.git $@

vendor/%:
	git clone git@github.com:brendanhay/$*.git $@

czar/src/Czar/Internal/Protocol.hs: lib/czar.proto
	hprotoc -I lib -p Czar.Internal -d czar/src -v $<

bin/%: bin
	@ln -fs ../dist/build/czar-$*/czar-$* $@

bin:
	mkdir -p bin
