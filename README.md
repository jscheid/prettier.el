# prettier.el

The `prettier` Emacs package reformats your code by running
[Prettier](https://github.com/prettier/prettier) with minimal overhead,
by request or transparently on file save.

Overhead on top of Prettier registers in the range of single-digit
milliseconds for reasonably sized files on a decent system.

Prettier is run in a long-running process so that Node startup overhead
is only paid once, and so that V8 JIT compilation has a better
opportunity to work its magic.

Additional features include first-class support for reformatting remote
files via `tramp-mode` and integration with many major modes.

This is not the official Prettier integration for Emacs. The official
package can be found at
[https://github.com/prettier/prettier-emacs](https://github.com/prettier/prettier-emacs).

## Installation

This package is not yet on MELPA because it's not as stable as
`prettier-emacs`. For now, head to the [Releases
page](https://github.com/jscheid/prettier.el/releases/) and download a
tarball, then `M-x package-install-file` if you're feeling lucky.

A `node` executable needs to be on `exec-path`. It is recommended to
provide a recent version of Node since older versions tend to be
significantly slower. `prettier` also uses the Emacs `nvm` package so
that if you have [nvm](https://github.com/creationix/nvm), all you need
to do is run `nvm install node` to ensure the latest version is
available.

The Prettier package needs to be installed inside your npm package, or
globally with either `npm` or `yarn`. Version 1.6 is the minimum
requirement but it's recommended to use the latest version in order to
get access to all parsers, features and bugfixes.

You can use `M-x prettier-info` to see which versions are being used for
a given buffer and to aid with diagnosis when Node or Prettier can't be
found.

The above instructions are for reformatting local files; see below for
how to configure remote systems.

Use `M-x global-prettier-mode` to turn on the minor mode in all major
modes supported by your version of Prettier and by any plugins installed
(see [prettier-enabled-parsers](#prettier-enabled-parsers) below.) When
enabled, the minor mode will reformat on file save and also sync
settings from Prettier to Emacs, such as indentation level.

To enable `prettier-mode` globally at startup:

```elisp
(add-hook 'after-init-hook #'global-prettier-mode)
```

## Usage

When enabled as a minor mode with `prettier-mode`, your source code is
formatted on save.

You can also `M-x prettier-prettify` to prettify the whole buffer at any
time in any buffer (doing so doesn't require the minor mode to be
enabled), and `M-x prettier-prettify-region` to prettify region (but
note that region should align with a complete block of code).

If you give a prefix argument to either of these two commands you will
be able to set/override the Prettier parser to use for formatting.

### Changing Prettier Configuration

After you change Prettier options (by creating, modifying or deleting
Prettier configuration files), this package won't pick up the changes
automatically. Disable and re-enable `global-prettier-mode` to force
reloading the new configuration. This might be improved in the future by
monitoring relevant files for changes.

### On Remote Servers

When you edit files on a remote host, Prettier will be run remotely on
that host. It follows that the remote host needs to have Node and
Prettier installed as described above. `nvm` can't be used on remote
hosts; you will have to ensure `tramp-remote-path` is set correctly.

Remote formatting might be improved in the future in the following ways:

- Using remote Prettier only to determine options for the remote file,
  but using local Prettier to do the actual formatting. This would help
  with large source files and slow connections.

- Obviating the need for remote Prettier by walking the remote
  filesystem and finding configuration files. This is unlikely to happen
  because it would mean having to keep this package's code in sync with
  Prettier configuration discovery logic.

## Customization

```
M-x customize-group prettier
```

### `prettier-mode-sync-config-flag`

Set this to `nil` if you don't want `prettier-mode` to change your
buffer-local settings to match Prettier options.

### `prettier-editorconfig-flag`

Set this to `nil` if you don't want Prettier to use `.editorconfig`
files to load options.

### `prettier-enabled-parsers`

Customize this to match the Prettier version you're using and any
Prettier plugins you have installed.

The default setting assumes that you have a recent version of Prettier
without any plugins.

### `prettier-inline-errors-flag`

Set this to `t` if you want syntax errors to be shown inline instead of
sending them to the default error buffer. This feature is experimental
and won't yet work well when the error is outside the window.

### `prettier-pre-warm`

Choose how to pre-warm Prettier caches.

Essentially this selects when you wait for Prettier startup overhead:
with `none`, you tend to wait for it on first save. With `full`, you
wait when `prettier-mode` is first activated.

`some` is a compromise, with it you wait some on first activation and
some on first save.

## Differences from `prettier-emacs`

- `prettier-emacs` launches Prettier in an external process, paying
  the Node startup overhead on each invocation unless mitigated by tools
  such as `prettier_d`. This package uses a long-running child process.

- `prettier-emacs` lets Prettier guess the parser to use based on the
  filename. This package instead derives the parser from the major mode
  when possible and falls back to a guess based on the filename only
  when the major mode isn't supported.

- `prettier-emacs` doesn't touch your buffer configuration, such as
  indentation offsets. `prettier-mode`, by default, tries to set the
  buffer configuration to match Prettier configuration as closely as
  possible.

- This package keeps point at the same location relative to your code.

- This package has first-class support for editing remote files via
  `tramp-mode` (with some room for improvement for large source files
  and slow connections), and for using a local Prettier installation (in
  your project's `node_modules` rather than installed globally.)

- `prettier-emacs` allows overriding various Prettier options by setting
  `prettier-js-args` or `prettier-js-width-mode`. In contrast, this
  package believes that your Prettier configuration files should serve
  as the single source of truth.

## Credits

This package is heavily inspired by the original package that integrated
Prettier into Emacs, `prettier-emacs`. Thanks to James Long, Roman Coedo
and all contributors.

Of course, the amazing Prettier itself is what does all the heavy
lifting here. Thanks again to James and to the many people who have made
Prettier into what it is today.

## License

© Copyright 2018-present Julian Scheid

GNU Public License v3 or any later version (see [COPYING](COPYING))
