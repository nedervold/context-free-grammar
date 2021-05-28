.PHONY : all
all :
	cabal build

.PHONY : test
test :
	cabal test

################################
# department of sanitation
################################

.PHONY : clean
clean :
	cabal clean
	# remove emacs cruft
	-find . -name '*~' -delete
	-find . -name '\#*' -delete

################################
# distribution
################################

.PHONY : dist
dist : all
	-cabal check
	cabal sdist

TEMPDIR := $(shell mktemp -d /tmp/temp.XXXX)

.PHONY : dist-test
dist-test : dist
	$(eval DIR := $(shell (cabal info . | awk '{print $$2 ; exit}')))
	$(eval TARBALL := $(DIR).tar)
	$(eval TGZBALL := $(TARBALL).gz)
	cp dist-newstyle/sdist/$(TGZBALL) $(TEMPDIR)
	cd $(TEMPDIR) && gunzip $(TGZBALL) && tar -xf $(TARBALL)
	cd $(TEMPDIR)/$(DIR) && make test
	rm -rf $(TEMPDIR)

################################
# documentation
################################

.PHONY : docs
docs :
	cabal haddock --haddock-for-hackage

.PHONY : docs-open
docs-open : docs
	# I can't find a better way to find the haddocks than guessing.
	open `find dist-newstyle/ -name doc-index.html`


################################
# reformatting
################################

.PHONY : reformat
reformat :
	find src tests -name '*.hs' -exec hindent \{} \;

################################
# de-linting
################################

.PHONY : lint
lint :
	hlint src tests
