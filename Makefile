OCAML_VERSION := \
	$(shell ocamlc -version | grep -E -o '^[0-9]+\.[0-9]+' | sed 's/\.//')

ifeq ($(shell test $(OCAML_VERSION) -ge 402 && echo true),true)
SAFE_STRING := ,-safe-string
ifeq ($(shell ocamlfind query -qe -qo bisect_ppx && echo true),true)
COVERAGE := yes
COVERAGE_TAGS := \
	-tag-line '<src/*>: package(bisect_ppx)' \
	-tag-line '<test/*.native>: package(bisect_ppx)'
COVERAGE_DIR := coverage
endif
endif

ifeq ($(shell test $(OCAML_VERSION) -ge 400 && echo true),true)
BIN_ANNOT := ,-bin-annot
endif

ifdef TRAVIS_COMMIT
WERROR := ,-warn-error,+A-3
endif

CFLAGS := -cflags -w,+A$(BIN_ANNOT)$(SAFE_STRING)$(WERROR)

OCAMLBUILD := ocamlbuild -use-ocamlfind -no-links
DEP_TEST_DIR := test/dependency

.PHONY : build
build :
	$(OCAMLBUILD) $(CFLAGS) lambdasoup.cma lambdasoup.cmxa

BS4_MISSING := Beautiful Soup not installed. Skipping Python performance test.

.PHONY : test
test :
	@rm -f bisect*.out
	$(OCAMLBUILD) $(COVERAGE_TAGS) $(CFLAGS) test.native --
	@if [ "$(COVERAGE)" = yes ] ; then \
		bisect-ppx-report -I _build -html $(COVERAGE_DIR) bisect*.out ; \
		echo ; \
		bisect-ppx-report -text - -summary-only bisect*.out \
			| sed 's/Summary/Coverage/' ; \
		echo "See $(COVERAGE_DIR)/index.html for coverage report" ; \
	fi

.PHONY : performance-test
performance-test :
	$(OCAMLBUILD) $(CFLAGS) performance.native --
	$(OCAMLBUILD) $(CFLAGS) performance.byte --
	@((python -c "import bs4" 2> /dev/null \
		|| (echo $(BS4_MISSING); exit 1)) \
		&& (echo python test/performance.py; python test/performance.py)) \
		|| exit 0

.PHONY : reverse-dependency-test
reverse-dependency-test :
	cd $(DEP_TEST_DIR) && \
		$(OCAMLBUILD) -clean && \
		$(OCAMLBUILD) $(CFLAGS) dependency.native --

.PHONY : all-tests
all-tests : uninstall install test performance-test reverse-dependency-test

HTML := docs/html

.PHONY : docs
docs : docs-postprocess
	$(OCAMLBUILD) docs/soup.docdir/index.html
	rm -rf $(HTML)
	mkdir -p $(HTML)
	rsync -r _build/docs/soup.docdir/* $(HTML)/
	cp docs/style.css $(HTML)/
	rm $(HTML)/index*.html
	_build/docs/postprocess.native < $(HTML)/Soup.html > $(HTML)/index.html
	rm $(HTML)/Soup.html $(HTML)/*R*html $(HTML)/type_Soup.html $(HTML)/*.stamp
	@echo "\nSee docs/html/index.html"

.PHONY : docs-postprocess
docs-postprocess :
	$(OCAMLBUILD) postprocess.native

GHPAGES_REPO := scratch/docs-publish

.PHONY : publish-docs
publish-docs : docs
	@[ -d $(GHPAGES_REPO)/.git ] \
		|| (echo "\nPlease create a repository in $(GHPAGES_REPO)"; exit 1)
	cp $(HTML)/* $(GHPAGES_REPO)
	cd $(GHPAGES_REPO) \
		&& git add -A \
		&& git commit --amend --reset-author -m "Lambda Soup documentation." \
		&& git push -f

INSTALL := \
	_build/src/lambdasoup.cma _build/src/lambdasoup.cmxa \
	_build/src/lambdasoup.a _build/src/soup.cmi _build/src/soup.mli \
	_build/src/soup.cmti _build/src/soup.cmt _build/src/soup.cmx
PACKAGE := lambdasoup

.PHONY : ocamlfind-install
ocamlfind-install :
	ocamlfind install $(PACKAGE) -optional src/META $(INSTALL)

.PHONY : ocamlfind-uninstall
ocamlfind-uninstall :
	ocamlfind remove $(PACKAGE)

.PHONY : install
install :
	[ -f opam ] || ln -s src/opam
	opam pin add . -y

.PHONY : uninstall
uninstall :
	opam pin remove $(PACKAGE) -y

.PHONY : clean
clean :
	$(OCAMLBUILD) -clean
	cd $(DEP_TEST_DIR) && $(OCAMLBUILD) -clean
	rm -rf docs/html opam
