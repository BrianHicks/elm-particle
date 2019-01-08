# make thinks the -demo files are intermediate without this and removes
# them. Why? I dunno. But this fixes it. Thanks, The Internet™!
.SECONDARY:

docs: docs/tada.png docs/Firework.html docs/Confetti.html docs/Water.html
	touch -m $@

documentation.json: $(shell find src -name '*.elm')
	elm make --docs=$@

docs/tada.png: examples/tada.png
	@mkdir -p $(@D)
	cp $^ $@

docs/%-demo.html: examples/%.elm $(shell find src -name '*.elm')
	@mkdir -p $(@D)
	cd $(<D); elm make --optimize --output=../$@ $(<F)

docs/%.html: examples/%.elm docs/%-demo.html examples/build-doc-page.sh $(shell find src -name '*.elm')
	@mkdir -p $(@D)
	./examples/build-doc-page.sh $< > $@

.PHONY: ci
ci: docs documentation.json
	elm-format --validate $(shell find src examples -name '*.elm')

.PHONY: clean
clean:
	rm -rf docs
