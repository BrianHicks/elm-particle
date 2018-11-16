docs: docs/tada.png docs/Confetti.html docs/Water.html
	touch -m $@

docs/tada.png: examples/tada.png
	@mkdir -p $(@D)
	cp $^ $@

docs/%.html: examples/%.elm examples/build-doc-page.sh $(shell find src -name '*.elm')
	@mkdir -p $(@D)
	./examples/build-doc-page.sh $< > $@
