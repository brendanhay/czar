DEPS := vendor/ekg \
 vendor/options \
 czar/src/Czar/Internal/Protocol.hs

default: build

build: .conf
	cabal-dev build

install: $(DEPS)
	cabal-meta install --dev -j \
	 --disable-documentation \
	 --disable-library-coverage

clean:
	-rm -rf .conf dist vendor .shelly

lint:
	hlint czar-* system-info/src

.conf:
	cabal-dev configure && touch .conf

vendor/%:
	git clone git@github.com:brendanhay/$*.git $@

czar/src/Czar/Internal/Protocol.hs: lib/czar.proto
	hprotoc -I lib -p Czar.Internal -d czar/src -v $<
