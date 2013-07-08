DEPS := vendor/ekg \
 vendor/options \
 czar/src/Czar/Internal/Protocol.hs

install: $(DEPS)
	cabal-meta install --dev -j \
	 --disable-documentation \
	 --disable-library-coverage

vendor/%:
	git clone git@github.com:brendanhay/$*.git $@

czar/src/Czar/Internal/Protocol.hs: lib/czar.proto
	hprotoc -I lib -p Czar.Internal -d czar/src -v $<
