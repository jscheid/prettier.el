# Hacking prettier.el

## Running from source

To run prettier.el from source you will need zopfli (for minimizing JS
code). You also need `node` and `yarn`. Run `yarn` to install dependencies.

Run `make` in the root directory and then `M-x eval-buffer` in `prettier.el`.

## Changing JavaScript Source Code

After changes to JS code you need to run `make` again as well as `M-x prettier-kill-all-processes`.

## Creating a package

To creata a distribution package, you will additionally need the
following:

- pandoc
- python (to run pandoc filters)
- make, tar, makeinfo and install-info, emacs

Ensure that the version number in `prettier.el` is correct and run `make package`. This will generate a `tar.gz` file in the current directory.
