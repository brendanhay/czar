DEPS := vendor/ekg vendor/options src/Czar/Internal/Protocol

default: all

vendor/%:
	git clone git@github.com:brendanhay/$*.git $@ && \
	 cd $@ && \
	 cabal-dev install -s ../../../cabal-dev -j \
	 --disable-documentation \
	 --disable-library-coverage

src/Czar/Internal/Protocol: lib/czar.proto
	hprotoc -I lib -p Czar.Internal -d src -v $<

include ../lib/common.mk
