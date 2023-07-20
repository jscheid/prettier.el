SHELL = /bin/bash -o pipefail
PANDOC = pandoc
VERSION := $(shell emacs -Q -batch --eval='(progn (require '"'"'package) (find-file "prettier.el") (princ (package-version-join (package-desc-version (package-buffer-info)))))')

dist: build/ \
			dist/ \
			dist/COPYING \
			dist/COPYING-diff-match-patch \
			dist/prettier-pkg.el \
			dist/prettier.el \
			dist/prettier-el.js.gz.base64 \
			dist/bootstrap-min.js \
			dist/dir \
			dist/prettier.info

build/ dist/:
	mkdir -p $@

dist/%: %
	cp $< $@

dist/COPYING-diff-match-patch: node_modules/diff-match-patch/LICENSE
	cp $^ $@

dist/dir: dist/prettier.info
	rm -f $@
	install-info $< $@

dist/prettier.info: build/prettier.texi
	makeinfo --no-validate --force $< -o $@

build/prettier.texi: README.md metadata.yaml build-tools/ghm-to-texi.py
	${PANDOC} metadata.yaml $< -s --filter ./build-tools/ghm-to-texi.py -o $@

build/prettier-el-min.js: prettier-el.js externs.js
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
		--dependency_mode=NONE \
		--language_out=ECMASCRIPT_2017 \
		--output_wrapper='%output%;global.m'

dist/bootstrap-min.js: bootstrap.js externs.js
	node_modules/.bin/google-closure-compiler \
		--js_output_file=$@ \
		--module_resolution=NODE \
		--process_common_js_modules \
		--compilation_level=ADVANCED_OPTIMIZATIONS \
		--js \
		bootstrap.js \
		--externs externs.js \
		--dependency_mode=NONE \
		--language_out=ECMASCRIPT_2017

dist/prettier-el.js.gz.base64: build/prettier-el-min.js
	node -e "const fs = require('fs'); const zopfli = require('node-zopfli-es'); fs.createReadStream('$<').pipe(zopfli.createGzip()).pipe(process.stdout)" \
		| base64 --wrap=70 > $@

dist/prettier-pkg.el: prettier.el build-tools/create-pkg-el.el
	emacs -batch --load=./build-tools/create-pkg-el.el --eval='(prettier--create-pkg-el "$@")'

clean:
	rm -Rf build/ dist/
