#
# Vars
#

DEPS := vendor/ekg vendor/options czar/Czar/Internal/Protocol.hs

#
# Default
#

all: build lint

#
# General
#

build: .conf $(DEPS)
	cabal-dev build && \
	 $(MAKE) bin/czar-server bin/czar-agent bin/czar-graphite bin/czar-pagerduty

install: $(DEPS)
	cabal-meta install -j \
	 --dev \
	 --disable-documentation \
	 --disable-library-coverage

clean:
	-rm -rf dist .conf bin/czar-* $(DEPS) *.imports

#
# Test
#

lint:
	hlint czar-agent czar-server czar-graphite czar-pagerduty

#
# Patterns
#

.conf:
	cabal-dev configure && touch .conf

bin/%:
	@mkdir -p bin && ln -fs ../dist/build/$*/$* $@

czar/Czar/Internal/Protocol.hs: lib/czar.proto
	hprotoc -I lib -p Czar.Internal -d czar -v $<

vendor/%:
	git clone git@github.com:brendanhay/$*.git $@
