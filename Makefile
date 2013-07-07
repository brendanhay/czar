default: all

make = cd $(1) && $(MAKE) $@

.DEFAULT:
	$(call make,czar)
	$(call make,czar-server)
	$(call make,czar-agent)
	$(call make,czar-graphite)
	$(call make,czar-pagerduty)
	$(call make,czar-checks)

