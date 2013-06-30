#
# Vars
#

DEPS := vendor/ekg czar/Czar/Internal/Protocol.hs

#
# Default
#

all: build lint

#
# General
#

build: .conf $(DEPS)
	cabal-dev build && \
	 $(MAKE) bin/czar-server bin/czar-agent bin/czar-graphite

install: $(DEPS)
	cabal-meta install -j \
	 --dev \
	 --disable-documentation \
	 --disable-library-coverage


clean:
	-rm -rf .conf bin/czar-* $(DEPS)

#
# Test
#

lint:
	hlint czar-agent czar-server

#
# Patterns
#

.conf:
	cabal-dev configure && touch .conf

bin/%:
	@mkdir -p bin && ln -fs ../dist/build/$*/$* $@

czar/Czar/Internal/Protocol.hs: lib/czar.proto
	hprotoc -I lib -p Czar.Internal -d czar -v $<

vendor/ekg:
	git clone git@github.com:brendanhay/ekg.git $@
