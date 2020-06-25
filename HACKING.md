# Hacking prettier.el

## Build environment

You will need a fairly specific build environment which is helpfully provided by
the included [Vagrant](https://www.vagrantup.com/) box. Install Vagrant, then
run `vagrant up --provider=virtualbox` in this directory to initialize the
environment. Run the below commands prefixed by `./with-vagrant`.

If you would rather not use Vagrant, you can derive instructions for how to set
up your local build environment from `Vagrantfile`; if you do so, please
consider sending a PR with instructions.

## Base directory

Ensure that variable `prettier-el-home` is set to the source directory or
`prettier.el` might pick up the wrong version of the JavaScript files. One way
to do so is the following:

```elisp
(defvar prettier-el-home "/path/to/this/directory")
(use-package prettier :load-path prettier-el-home)
```

## Building

Initially, and after changes to JS code, you will need to build by running
`make`, then force a restart of the daemon process(es).

You can do so by toggling `M-x global-prettier-mode` off and back on; by
manually killing any `*prettier...*` buffers, or by running
`M-x eval-expression RET (prettier--quit-all-processes) RET`, whichever
you find works best for you. The main objective is for all `*prettier...*`
process buffers to be killed so that the next time you use it, a fresh daemon is
started.

## Linting and testing

To run the linters locally, follow a similar approach to the CI pipeline
defined in `.github/workflows/`, e.g. first set up a sandbox:

    ./makem.sh -v --sandbox=sandbox --install-{deps,linters}

and then run the linters:

    ./makem.sh -v --sandbox=sandbox lint

You can also run all tests:

    ./makem.sh -v --sandbox=sandbox test

or both linters and tests in one go:

    ./makem.sh -v --sandbox=sandbox all
