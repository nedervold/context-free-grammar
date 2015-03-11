.PHONY : clean configure maintainer-clean dist dist-test docs lint

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

dist-test : dist
	for TARBALL in $(wildcard dist/context-free-grammar-*.tar.gz) ; \
	    do \
		cp $$TARBALL /tmp ; \
		cd /tmp ; \
		gunzip context-free-grammar-*.tar.gz ; \
		tar -xf context-free-grammar-*.tar; \
		rm context-free-grammar-*.tar ; \
		cd context-free-grammar-* ; \
		make test ; \
		rm -rf /tmp/context-free-grammar-* ; \
	done

    # TODO dist-test should run in a different directory so if a test
    # fails, the new one doesn't collide with the detritus of the old

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
