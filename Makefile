.PHONY : all clean configure dist dist-test docs docs-open lint \
	maintainer-clean test

GEN_CODE = dist/build/generate-code/generate-code

all : configure
	cabal build

test : all
	cabal test

################################
# department of sanitation
################################

clean :
	cabal clean
	# remove emacs cruft
	-find . -name '*~' -delete
	-find . -name '\#*' -delete

maintainer-clean : clean
	-cabal sandbox delete
	-rm cabal.config

################################
# the cabal does not exist
################################

cabal.config :
	echo 'tests: True' > cabal.config

.cabal-sandbox :
	cabal sandbox init

configure : .cabal-sandbox cabal.config
	cabal install --dependencies-only
	cabal configure

################################
# distribution
################################

dist : all
	-cabal check
	cabal sdist

TEMPDIR := $(shell mktemp -d /tmp/temp.XXXX)

dist-test : dist
	$(eval DIR := $(shell (cabal info . | awk '{print $$2 ; exit}')))
	$(eval TARBALL := $(DIR).tar)
	$(eval TGZBALL := $(TARBALL).gz)
	echo $(TEMPDIR)
	cp dist/$(TGZBALL) $(TEMPDIR)
	cd $(TEMPDIR) && gunzip $(TGZBALL) && tar -xf $(TARBALL)
	cd $(TEMPDIR)/$(DIR) && make test
	rm -rf $(TEMPDIR)

################################
# documentation
################################

docs : configure
	cabal haddock

docs-open : docs
	open dist/doc/html/context-free-grammar/index.html

################################
# generate-code
################################

$(GEN_CODE) : configure
	cabal build generate-code

################################
# de-linting
################################

lint :
	hlint -i 'Use import/export shortcut' src tests
	# The import/export shortcut plays poorly with Haddock
