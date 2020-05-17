PANDOC := ${HOME}/.local/bin/pandoc

package: README COPYING COPYING-diff-match-patch prettier-pkg.el prettier.el prettier-el.js.gz.base64 bootstrap-min.js dir prettier.info
	$(eval VERSION := $(shell emacs -batch --eval='(progn (require '"'"'package) (find-file "prettier.el") (princ (package-version-join (package-desc-version (package-buffer-info)))))'))
	env COPYFILE_DISABLE=true gtar --transform 's,^,prettier-${VERSION}/,' -cf prettier-$(VERSION).tar $^

COPYING-diff-match-patch: node_modules/diff-match-patch/LICENSE
	cp $^ $@

dir: prettier.info
	rm -f $@
	install-info $< $@

prettier.info: prettier.texi
	makeinfo --no-validate --force $< -o $@

prettier.texi: README.md metadata.yaml build-tools/ghm-to-texi.py
	${PANDOC} metadata.yaml $< -s --filter ./build-tools/ghm-to-texi.py -o $@

README: README.md build-tools/ghm-to-md.py
	${PANDOC} $< -f gfm -s -t markdown_strict --filter ./build-tools/ghm-to-md.py -o $@

prettier-pkg.el: prettier.el build-tools/create-pkg-el.el
	emacs -batch -l ./build-tools/create-pkg-el.el

prettier-el-min.js: prettier-el.js externs.js
	node_modules/.bin/google-closure-compiler \
		--js_output_file=$@ \
		--module_resolution=NODE \
		--process_common_js_modules \
		--compilation_level=ADVANCED_OPTIMIZATIONS \
		--js \
		prettier-el.js \
		node_modules/diff-match-patch/index.js \
		--externs externs.js \
		--hide_warnings_for=node_modules/diff-match-patch \
		--dependency_mode=NONE

bootstrap-min.js: bootstrap.js externs.js
	node_modules/.bin/google-closure-compiler \
		--js_output_file=$@ \
		--module_resolution=NODE \
		--process_common_js_modules \
		--compilation_level=ADVANCED_OPTIMIZATIONS \
		--js \
		bootstrap.js \
		--externs externs.js \
		--dependency_mode=NONE

prettier-el.js.gz.base64: prettier-el-min.js
	zopfli -c $< | base64 --wrap=70 > $@

clean:
	rm -f *-min.js *.base64 *.gz *.tar *.sig *.info *.texi *.elc prettier-pkg.el dir README
