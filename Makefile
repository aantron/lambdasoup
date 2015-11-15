BUILD_DIR := build
CLFAGS := -clfags -safe-string,-w,+A-48,-short-paths
OCAMLBUILD := ocamlbuild -use-ocamlfind -build-dir $(BUILD_DIR)
DEP_TEST_DIR := test/dependency

.PHONY : build
build :
	$(OCAMLBUILD) $(CFLAGS) lambdasoup.cma lambdasoup.cmxa

BS4_MISSING := Beautiful Soup not installed. Skipping Python performance test.

.PHONY : test
test :
	$(OCAMLBUILD) $(CFLAGS) test.native --
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

HTML := docs/html

.PHONY : docs
docs : docs-postprocess
	$(OCAMLBUILD) docs/soup.docdir/index.html
	rm -rf $(HTML)
	mkdir -p $(HTML)
	rsync -r build/docs/soup.docdir/* $(HTML)/
	cp docs/style.css $(HTML)/
	rm $(HTML)/index*.html
	build/docs/postprocess.native < $(HTML)/Soup.html > $(HTML)/index.html
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
	build/src/lambdasoup.cma build/src/lambdasoup.cmxa build/src/lambdasoup.a \
	build/src/soup.cmi build/src/soup.mli
PACKAGE := lambdasoup

.PHONY : ocamlfind-install
ocamlfind-install :
	ocamlfind install $(PACKAGE) src/META $(INSTALL)

.PHONY : ocamlfind-uninstall
ocamlfind-uninstall :
	ocamlfind remove $(PACKAGE)

.PHONY : install
install :
	[ -f opam ] || ln -s src/opam
	opam pin add . -y

.PHONY : uninstall
uninstall :
	opam pin remove lambdasoup -y

.PHONY : clean
clean :
	$(OCAMLBUILD) -clean
	cd $(DEP_TEST_DIR) && $(OCAMLBUILD) -clean
	rm -rf docs/html opam
