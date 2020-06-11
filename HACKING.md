# Hacking prettier.el

## Running from source

To run prettier.el from source you will need zopfli (for minimizing JS
code). You also need `node` and `yarn`. Run `yarn` to install dependencies.

Run `make` in the root directory and then `M-x eval-buffer` in `prettier.el`.

## Changing JavaScript Source Code

After changes to JS code you need to run `make` again as well as `M-x prettier-kill-all-processes`.

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

## Creating a package

To creata a distribution package, you will additionally need the
following:

- pandoc
- python (to run pandoc filters)
- make, tar, makeinfo and install-info, emacs

Ensure that the version number in `prettier.el` is correct and run `make package`. This will generate a `tar.gz` file in the current directory.
