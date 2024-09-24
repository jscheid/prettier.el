# Hacking prettier.el

## Build environment

You will need a fairly specific build environment which is helpfully provided by
the included [Vagrant](https://www.vagrantup.com/) box. Install Vagrant, then
run `vagrant up --provider=virtualbox` in this directory to initialize the
environment. Run the below commands prefixed by `./with-vagrant`.

### Building locally

Building locally is not tested thoroughly and Vagrant is the recommended
way. However, requirements are provided below.

#### Requirements

:warning: As the JS tooling evolves the documentation below might get outdated :warning:

* [node-zopfli requirements](https://github.com/pierreinglebert/node-zopfli?tab=readme-ov-file#prerequisites-for-building)
   and specifically a working [node-gyp](https://github.com/nodejs/node-gyp#installation) installation.
* `pandoc` and matching [panflute version](https://github.com/sergiocorreia/panflute?tab=readme-ov-file#note-on-versions).

## Building

Initially, and after changes to JS code, you will need to build by running
`make`, then force a restart of the daemon process(es) by running
`prettier-restart`.

## Linting and testing

To run the linters locally, follow a similar approach to the CI pipeline
defined in `.github/workflows/`, i.e. first set up a sandbox:

    ./makem.sh -v --sandbox=sandbox --install-{deps,linters}

and then run the linters:

    ./makem.sh -v --sandbox=sandbox lint

You can also run all tests:

    ./makem.sh -v --sandbox=sandbox test

or both linters and tests in one go:

    ./makem.sh -v --sandbox=sandbox all
