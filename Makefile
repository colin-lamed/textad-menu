STORY = captainfate

INSTALL_ROOT = $(shell stack path --stack-yaml=$(STORY)-js/stack.yaml --local-install-root)
OUT_PATH = $(INSTALL_ROOT)/bin/$(STORY)-js.jsexe

all: build

build:
	set -e

	stack build --stack-yaml=$(STORY)/stack.yaml
	stack build --stack-yaml=$(STORY)-js/stack.yaml

clean:
	stack clean --stack-yaml=$(STORY)/stack.yaml
	rm -R $(STORY)/.stack-work/install/
	stack clean --stack-yaml=$(STORY)-js/stack.yaml
	rm -R $(STORY)-js/.stack-work/install/

test:
	stack test --stack-yaml=$(STORY)/stack.yaml

run:
	firefox $(OUT_PATH)/index.html
