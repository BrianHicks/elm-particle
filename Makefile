# make thinks the -demo files are intermediate without this and removes
# them. Why? I dunno. But this fixes it. Thanks, The Internet™!
.SECONDARY:

docs: docs/tada.png docs/Confetti.html docs/Water.html
	touch -m $@

docs/tada.png: examples/tada.png
	@mkdir -p $(@D)
	cp $^ $@

docs/%-demo.html: examples/%.elm
	@mkdir -p $(@D)
	cd $(<D); elm make --optimize --output=../$@ $(<F)

docs/%.html: examples/%.elm docs/%-demo.html examples/build-doc-page.sh $(shell find src -name '*.elm')
	@mkdir -p $(@D)
	./examples/build-doc-page.sh $< > $@

.PHONY: clean
clean:
	rm -rf docs
