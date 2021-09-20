;;; prettier.el --- Code formatting with Prettier  -*- lexical-binding: t; -*-

;; Copyright (c) 2018-present Julian Scheid

;; Author: Julian Scheid <julians37@gmail.com>
;; Version: 1.1.0
;; Created: 7 Nov 2018
;; Keywords: convenience, languages, files
;; Homepage: https://github.com/jscheid/prettier.el
;; Package-Requires: ((emacs "26.1") (iter2 "0.9") (nvm "0.2"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Reformats your code by running Prettier on file save or on request,
;; with minimal overhead.  By default, adjusts buffer-local
;; indentation settings and such to match Prettier config when used as
;; a minor mode.

;; See Info manual or README for further details.


;;; Code:


;;;; Requirements

(require 'iter2)
(require 'json)
(require 'nvm)
(require 'tramp)
(require 'subr-x)
(require 'compile)
(require 'ansi-color)
(require 'package)

(eval-when-compile
  (require 'cl-lib)
  (require 'rx)

  (defun prettier--readme-link (anchor)
    "Return the URL of the Readme section identified by ANCHOR."
    (concat "https://github.com/jscheid/prettier.el#"
            anchor)))


;;;; Customization

(defgroup prettier nil
  "Code reformatting using Prettier."
  :group 'files
  :prefix "prettier"
  :link '(url-link :tag "Repository"
                   "https://github.com/jscheid/prettier.el"))

(defcustom prettier-pre-warm 'full
  "Choose how to pre-warm Prettier caches.

Essentially this selects when you wait for Prettier startup
overhead: with `none', you tend to wait for it on first save.
With `full', you wait when command `prettier-mode' is first
activated.  `some' is a compromise, with it you wait some on
first activation and some on first save."
  :type '(choice
          (const :tag "No pre-warming, everything on-demand" none)
          (const :tag "Start server early, no other pre-warming" some)
          (const :tag "Pre-warm as much as possible" full))
  :package-version '(prettier . "0.1.0")
  :group 'prettier
  :link '(info-link "(prettier)prettier-pre-warm")
  :link `(url-link ,(eval-when-compile
                      (prettier--readme-link
                       "prettier-pre-warm"))))

(defcustom prettier-inline-errors-flag nil
  "Non-nil means to show Prettier errors inline using overlays.

When non-nil, create an overlay under the line with the error to
show the message, aligned with the column.  This doesn't
currently work well when the error is outside window.

When nil, send errors to the default error buffer."
  :type 'boolean
  :package-version '(prettier . "0.1.0")
  :group 'prettier
  :link '(info-link "(prettier)prettier-inline-errors-flag")
  :link `(url-link ,(eval-when-compile
                      (prettier--readme-link
                       "prettier-inline-errors-flag"))))


(defcustom prettier-mode-sync-config-flag t
  "Non-nil means to attempt syncing Prettier configuration to Emacs."
  :type 'boolean
  :package-version '(prettier . "0.1.0")
  :group 'prettier
  :link '(info-link "(prettier)prettier-mode-sync-config-flag")
  :link `(url-link ,(eval-when-compile
                      (prettier--readme-link
                       "prettier-mode-sync-config-flag"))))
;;;###autoload
(put 'prettier-mode-sync-config-flag 'safe-local-variable 'booleanp)

(defcustom prettier-editorconfig-flag t
  "Non-nil means to use `.editorconfig' files when present.

Requires Prettier 1.9+."
  :type 'boolean
  :package-version '(prettier . "0.1.0")
  :group 'prettier
  :link '(info-link "(prettier)prettier-editorconfig-flag")
  :link `(url-link ,(eval-when-compile
                      (prettier--readme-link
                       "prettier-editorconfig-flag"))))
;;;###autoload
(put 'prettier-editorconfig-flag 'safe-local-variable 'booleanp)

(defcustom prettier-infer-parser-flag t
  "Non-nil means to fall back to inferring a parser."
  :type 'boolean
  :package-version '(prettier . "0.5.0")
  :group 'prettier
  :link '(info-link "(prettier)prettier-infer-parser-flag")
  :link `(url-link ,(eval-when-compile
                      (prettier--readme-link
                       "prettier-infer-parser-flag"))))
;;;###autoload
(put 'prettier-infer-parser-flag 'safe-local-variable 'booleanp)

(defcustom prettier-enabled-parsers '(angular
                                      babel
                                      babel-flow
                                      babel-ts
                                      css
                                      elm
                                      espree
                                      flow
                                      graphql
                                      html
                                      java
                                      json
                                      json5
                                      json-stringify
                                      less
                                      lua
                                      markdown
                                      mdx
                                      meriyah
                                      php
                                      postgresql
                                      pug
                                      python
                                      ruby
                                      scss
                                      sh
                                      solidity
                                      svelte
                                      swift
                                      toml
                                      typescript
                                      vue
                                      xml
                                      yaml)
  "Prettier parsers to enable.

A disabled parser won't be used unless
`prettier-infer-parser-flag' is non-nil and Prettier falls back
on it.  Enabled parsers are not necessarily available, depending
on your Prettier version and which plug-ins you have installed."
  :type
  '(set
    (const :tag "Angular (1.15+)" angular)
    (const :tag "Babel (formerly Babylon)" babel)
    (const :tag "Babel-Flow (1.15+)" babel-flow)
    (const :tag "Babel-TS (2.0+)" babel-ts)
    (const :tag "CSS (1.4+)" css)
    (const :tag "Elm (2.0+?, requires plugin)" elm)
    (const :tag "Espree (2.2+)" espree)
    (const :tag "Flow" flow)
    (const :tag "GraphQL (1.5+)" graphql)
    (const :tag "Java (2.0+?, requires plugin)" java)
    (const :tag "JSON (1.5+)" json)
    (const :tag "JSON 5 (1.5+)" json5)
    (const :tag "JSON.stringify (1.5+)" json-stringify)
    (const :tag "LESS (1.4+)" less)
    (const :tag "Lua (1.10+, requires plugin)" lua)
    (const :tag "HTML (1.16+)" html)
    (const :tag "Markdown (1.8+)" markdown)
    (const :tag "MDX (1.15+)" mdx)
    (const :tag "Meriyah (2.2+)" meriyah)
    (const :tag "PHP (1.10+, requires plugin)" php)
    (const :tag "PostgreSQL (1.10+, requires plugin" postgresql)
    (const :tag "Pug (2.0+, requires plugin)" pug)
    (const :tag "Python (1.10+, requires plugin)" python)
    (const :tag "Ruby (1.10+, requires plugin)" ruby)
    (const :tag "SCSS (1.4+)" scss)
    (const :tag "Shell (2.0+)" sh)
    (const :tag "Solidity (2.0+?, requires plugin)" solidity)
    (const :tag "Svelte (1.16+, requires plugin)" svelte)
    (const :tag "Swift (1.10+, requires plugin)" swift)
    (const :tag "TOML (1.16+, requires plugin)" toml)
    (const :tag "TypeScript (1.4+)" typescript)
    (const :tag "Vue (1.10+)" vue)
    (const :tag "XML (1.10+, requires plugin)" xml)
    (const :tag "YAML (1.14+)" yaml))
  :package-version '(prettier . "0.1.0")
  :group 'prettier
  :link '(info-link "(prettier)prettier-enabled-parsers")
  :link `(url-link ,(eval-when-compile
                      (prettier--readme-link
                       "prettier-enabled-parsers"))))

(defcustom prettier-mode-ignore-buffer-function
  #'prettier--in-node-modules-p
  "A function called to selectively ignore certain buffers.

The function should return non-nil if command `prettier-mode'
should not be enabled for the current buffer."
  :type 'function
  :package-version '(prettier . "0.2.0")
  :group 'prettier
  :link '(info-link "(prettier)prettier-ignore-buffer-function")
  :link `(url-link ,(eval-when-compile
                      (prettier--readme-link
                       "prettier-ignore-buffer-function"))))

(defcustom prettier-lighter
  '(:eval
    (concat
     " Prettier"
     (when (and prettier-last-parser prettier-version)
       (format "[%s:%s]" prettier-last-parser prettier-version))))
  "Mode line lighter for Prettier.

The value of this variable is a mode line template as in
`mode-line-format'.  See Info Node `(elisp)Mode Line Format' for
more information.  Note that it should contain a _single_ mode
line construct only.

Customize this variable to change how Prettier reports its status
in the mode line.

Set this variable to nil to disable the mode line completely."
  :type 'sexp
  :package-version '(prettier . "0.5.0")
  :group 'prettier
  :risky t
  :link '(info-link "(prettier)prettier-lighter")
  :link `(url-link ,(eval-when-compile
                      (prettier--readme-link
                       "prettier-lighter"))))

(defface prettier-inline-error
  '((t :inherit compilation-error))
  "Prettier face for errors."
  :package-version '(prettier . "0.1.0")
  :group 'prettier)


;;;; Non-customizable

(defconst prettier-benign-errors
  '("Error: Couldn't resolve parser")
  "Errors in this list are shown in the echo area.

Other errors are shown inline or in the error buffer.")

(defconst prettier-sync-settings
  '(((fill-column                   ; built-in
      js3-max-columns)              ; js3-mode
     :printWidth)

    ((enh-ruby-indent-tabs-mode     ; enh-ruby-mode
      indent-tabs-mode              ; built-in
      js3-indent-tabs-mode          ; js3-mode
      ruby-indent-tabs-mode)        ; ruby-mode
     :useTabs)

    ((c-basic-offset                ; cc-mode, java-mode
      css-indent-offset             ; css-mode, scss-mode etc
      elm-indent-offset             ; elm-mode
      enh-ruby-indent-level         ; enh-ruby-mode
      graphql-indent-level          ; graphql-mode
      handlebars-basic-offset       ; handlebars-mode
      js-indent-level               ; js-mode
      js2-basic-offset              ; js2-mode
      js3-indent-level              ; js3-mode
      lua-indent-level              ; lua-mode
      nxml-attribute-indent         ; nxml-mode
      nxml-child-indent             ; nxml-mode
      nxml-outline-child-indent     ; nxml-mode
      pug-tab-width                 ; pug-mode
      python-indent                 ; python-mode
      ruby-indent-level             ; ruby-mode
      sgml-basic-offset             ; js2-mode, html-mode, svelte-mode
      sh-indentation                ; sh-mode
      smie-indent-basic             ; smie.el (generic)
      standard-indent               ; indent.el (generic)
      swift-mode:basic-offset       ; swift-mode.el
      tab-width                     ; built-in
      typescript-indent-level       ; typescript-mode
      web-mode-code-indent-offset   ; web-mode
      web-mode-css-indent-offset    ; web-mode
      yaml-indent-offset)           ; yaml-mode
     :tabWidth)

    ((js-indent-first-init)
     nil)

    ;; Unless prettier has trailing commas disabled, don't warn
    ;; about their presence
    ((js2-strict-trailing-comma-warning
      js3-strict-trailing-comma-warning)
     :trailingComma
     (lambda (trailing-comma)
       (pcase trailing-comma
         ("es5" nil)
         ("all" nil)
         (_ 'unchanged))))

    ;; When prettier has semicolons disabled, don't warn
    ;; about their absence
    ((js2-strict-missing-semi-warning
      js3-strict-missing-semi-warning)
     :semi
     (lambda (semi)
       (if semi 'unchanged nil)))

    ((web-mode-auto-quote-style)
     :singleQuote
     (lambda (single-quote)
       (if single-quote 2 1)))

    ;; Force dtrt-indent mode off when we're controlling local config
    ((dtrt-indent)
     nil))
  "Settings to sync from Prettier to Emacs configuration.

A list of lists of two or three elements:

  `(VAR-LIST SOURCE-CONFIGURATION [TRANSFORM-FUNCTION])'

VAR-LIST is a list of Emacs variables to set.

SOURCE-CONFIGURATION is either a keyword that specifies which
Prettier configuration option to use for setting the Emacs
variables, or - when not a keyword - a static value to set the
variables to.

TRANSFORM-FUNCTION is an optional function; when present, it is
called with the value of the Prettier option and the result is
used for setting the Emacs variables, unless it is the symbol
`unchanged'.  If that symbol is returned, the Emacs variables
won't be touched.")

(eval-when-compile
  (defconst prettier-error-rx
    '(and (submatch (minimal-match
                     (zero-or-more (not cntrl))))
          (zero-or-more " ")
          "("
          (submatch (one-or-more digit))
          ":"
          (submatch (one-or-more digit))
          ")")))

(defun prettier--guess-js-ish ()
  "Return which parsers to use for a buffer with a JS-like mode."
  (cond
   ((or (and (boundp 'tide-mode)
             tide-mode)
        (and (fboundp 'lsp-buffer-language)
             (ignore-errors
               (member (lsp-buffer-language)
                       '("typescript" "typescriptreact")))))
    '(typescript babel-ts babel meriyah espree flow babel-flow))
   ((and (boundp 'flow-minor-mode)
         flow-minor-mode)
    '(babel-flow flow babel meriyah espree))
   (t
    '(babel meriyah espree flow babel-flow))))

(defconst prettier-major-mode-parsers
  `((angular-mode . (angular))
    (elm-mode . (elm))
    (svelte-mode . (svelte html))
    (html-mode . (html))
    (mhtml-mode . (html))
    (java-mode . (java))
    (js-mode . ,#'prettier--guess-js-ish)
    (js2-mode . ,#'prettier--guess-js-ish)
    (js3-mode . ,#'prettier--guess-js-ish)
    (typescript-mode . (typescript babel-ts))
    (css-mode . (css))
    (scss-mode . (scss))
    (less-mode . (less))
    (json-mode . (lambda ()
                   (if (and
                        buffer-file-name
                        (seq-contains
                         '("package.json"
                           "package-lock.json"
                           "composer.json")
                         (file-name-nondirectory buffer-file-name)))
                       '(json-stringify json json5)
                     '(json json5 json-stringify))))
    (graphql-mode . (graphql))
    (markdown-mode . (markdown))
    (nxml-mode . (xml))
    (pug-mode . (pug))
    (solidity-mode . (solidity))
    (toml-mode . (toml))
    (vue-mode . (vue))
    (yaml-mode . (yaml))
    (lua-mode . (lua))
    (ruby-mode . (ruby))
    (enh-ruby-mode . (ruby))
    (python-mode . (python))
    (php-mode . (php))
    (sh-mode . (sh))
    (sql-mode . (postgresql))
    (swift-mode . (swift)))
  "Map from major mode to Prettier parsers.

In each element, car is the mode and cdr is either a list of
parser names as symbols, or a function (without arguments) that,
when called with buffer current, returns such a list.")

(defconst prettier-web-mode-content-type-parsers
  `((nil . (html))
    ("javascript" . ,#'prettier--guess-js-ish)
    ("jsx" . ,#'prettier--guess-js-ish)
    ("typescript" . (typescript babel-ts))
    ("css" . (css))
    ("json" . (json json5))
    ("markdown" . (markdown))
    ("ruby" . (ruby))
    ("sql" . (postgresql)))
  "Map from `web-mode' content type to Prettier parsers.

In each element, car is the mode and cdr is either a list of
parser names as symbols, or a function (without arguments) that,
when called with buffer current, returns such a list.")


;;;; Variables

(defvar prettier-el-home (file-name-directory
                          (or load-file-name buffer-file-name))
  "Directory with `prettier.el' and auxiliary files.")

(defvar prettier-error-regex
  (eval-when-compile
    (rx-to-string prettier-error-rx))
  "Regular expression to use for parsing Prettier errors.")

(defvar prettier-compilation-regexps
  (eval-when-compile
    `(,(rx-to-string
        `(and
          line-start
          (submatch (one-or-more (not (any ":" cntrl))))
          ":"
          (zero-or-more " ")
          ,prettier-error-rx))
      1 3 4 nil 2))
  "Specifications for matching errors in prettier invocations.
See `compilation-error-regexp-alist' for help on their format.")

(defvar prettier-error-buffer-name
  "*prettier errors*"
  "Name to use for the buffer showing Prettier error messages.")

(defvar prettier-keep-server-buffer-flag nil
  "Non-nil means not to kill server buffer when process ends.

For debugging only.")

(defvar prettier-show-benchmark-flag nil
  "Non-nil means to show timing information.

For debugging and performance tuning only.")

(defvar prettier-timeout-seconds 20
  "Number of seconds before aborting and restarting Prettier.")

(defvar prettier-processes (make-hash-table :test 'equal)
  "Keep track of running node processes, keyed by `node-command'.
It's the name or path of the node executable `prettier--find-node'
returns.")

(defvar prettier-nvm-node-command-cache nil
  "Cache for the result of `prettier--node-from-nvm'.")

(defvar prettier-parser-history nil
  "History for `prettier--read-parsers'.")

;;;;; Local Variables

(defvar-local prettier-parsers nil
  "Non-nil means to force Prettier to use these parsers.

The first parser (in list order) supported by the Prettier
version or any plug-ins will be used.  If none of the given
parsers is supported, Prettier will fall back to inferring a
parser unless `prettier-infer-parser-flag' is nil.")
;;;###autoload
(put 'prettier-parsers 'safe-local-variable 'listp)

(defvar-local prettier-previous-local-settings nil
  "Used to backup settings so they can be restored later.")

(defvar-local prettier-error-overlay nil
  "Used to remember the last error overlay")

(defvar-local prettier-last-error-marker nil
  "Used to remember the last error marker")

(defvar-local prettier-last-parser nil
  "The last parser used to format the whole file")

(defvar-local prettier-version nil
  "The Prettier version used for this buffer")

;;;;; Imported Variables

;; For interop with web-mode.el
(defvar web-mode-content-type)


;;;; Commands

;;;###autoload
(defun prettier-prettify ()
  "Prettify the whole current buffer, or the part it is narrowed to.

With prefix, ask for the parser to use"
  (interactive "*")
  (prettier--prettify
   (or (when current-prefix-arg
         (prettier--read-parsers))
       (prettier--parsers))))

;;;###autoload
(defun prettier-prettify-region ()
  "Prettify the current region.

With prefix, ask for the parser to use"
  (interactive "*")
  (prettier--prettify
   (or (when current-prefix-arg
         (prettier--read-parsers))
       (prettier--parsers))
   (region-beginning)
   (region-end)))

(defun prettier--quit-all-processes ()
  "Quit all Prettier sub-processes."
  (maphash (lambda (_key process)
             (quit-process process))
           prettier-processes)
  (setq prettier-nvm-node-command-cache nil))

(defun prettier-restart ()
  "Restart Prettier in all buffers.

This will cause all caches to be cleared and the latest version
of the sidecar JavaScript file to be used.  It is executed every
time this package is loaded which is intended to ensure you're
running the latest when the package is upgraded.

You should run this function whenever any relevant configuration
changes, such as when you install a new version of Node,
Prettier, or any plugins; when you install or uninstall Prettier
as a local npm package in a directory from which you already have
files open in Emacs; or when you change Prettier settings that
might affect any open files."
  (interactive)
  (prettier--quit-all-processes)
  (let* (wait-timer
         (callback
          (lambda ()
            (when (zerop (hash-table-count prettier-processes))
              (cancel-timer wait-timer)
              (unless (eq prettier-pre-warm 'none)
                (mapc (lambda (buf)
                        (with-current-buffer buf
                          (when (and (boundp 'prettier-mode)
                                     prettier-mode)
                            (prettier--get-process
                             (eq prettier-pre-warm 'full)))))
                      (buffer-list)))
              (message "Prettier restart complete.")))))
    (setq wait-timer (run-with-timer 0.1 0.1 callback))))

(defun prettier--buffer-remote-p (&optional identification connected)
  "Return `file-remote-p' result for the current buffer.

IDENTIFICATION and CONNECTED have the same meaning as
`file-remote-p'."
  (and buffer-file-name
       (apply #'file-remote-p
              buffer-file-name
              identification
              connected)))

(defun prettier--pkg-version ()
  "Return the version of the `prettier' package."
  (package-version-join
   (package-desc-version
    (with-temp-buffer
      (let ((src (or
                  ;; load-file-name seemed like it would be useful
                  ;; here, but didn't work in practice.
                  (locate-library "prettier.el")
                  ;; This one shouldn't be needed:
                  (concat prettier-el-home "/prettier.el"))))
        (insert-file-contents src))
      (package-buffer-info)))))

(defun prettier-info ()
  "Show a temporary buffer with diagnostic info.

Can be used when there is a problem finding Node or Prettier, and
should be used when filing bug reports."
  (interactive)
  (let ((info
         (list
          :emacs-version (emacs-version)
          :prettier-el-version (prettier--pkg-version)
          :buffer-file-name buffer-file-name
          :remote-id (prettier--buffer-remote-p)
          :major-mode major-mode
          :exec-path exec-path
          :env process-environment
          :prettier-options
          (condition-case err
              (prettier--load-config)
            (error (print err))))))
    (with-current-buffer (get-buffer-create "prettier-info.el")
      (setq buffer-read-only nil)
      (erase-buffer)
      (princ
       ";; Please create a Gist with the contents of this buffer.\n
;; MAKE SURE TO REMOVE ANY SENSITIVE INFORMATION FIRST\n\n"
       (current-buffer))
      (pp info (current-buffer))
      (setq buffer-read-only t)
      (emacs-lisp-mode)
      (goto-char (point-min))
      (display-buffer (current-buffer)))))


;;;;; Modes

;;;###autoload
(define-minor-mode prettier-mode
  "Runs prettier on file save when this mode is turned on"
  :lighter prettier-lighter
  (if prettier-mode
      (progn
        (unless (eq prettier-pre-warm 'none)
          (prettier--get-process
           (eq prettier-pre-warm 'full)))
        (when prettier-mode-sync-config-flag
          (prettier--maybe-sync-config)
          (add-hook 'after-change-major-mode-hook
                    #'prettier--maybe-sync-config
                    'append
                    'local))
        (add-hook 'before-save-hook
                  #'prettier-prettify
                  nil
                  'local))
    (remove-hook 'before-save-hook
                 #'prettier-prettify
                 'local)
    (remove-hook 'after-change-major-mode-hook
                 #'prettier--maybe-sync-config
                 'local)

    (prettier--revert-synced-config)

    (setq prettier-last-parser nil
          prettier-last-error-marker nil
          prettier-version nil
          prettier-previous-local-settings nil)))

;;;###autoload
(define-globalized-minor-mode
  global-prettier-mode
  prettier-mode
  (lambda ()
    (when (and (not prettier-mode)
               (or (null prettier-mode-ignore-buffer-function)
                   (not (funcall
                         prettier-mode-ignore-buffer-function)))
               (prettier--parsers))
      (with-temp-message
          (unless (eq prettier-pre-warm 'none)
            "Prettier pre-warming...")
        (prettier-mode))))
  :group 'prettier)

(add-hook
 'global-prettier-mode-hook
 (lambda ()
   (unless global-prettier-mode
     (prettier--quit-all-processes))))


;;;; Support

(defun prettier--in-node-modules-p ()
  "Return t if current buffer's file is beneath `node_modules'."
  (and buffer-file-name
       (string-match "/node_modules/" buffer-file-name)))

(defun prettier--read-parsers ()
  "Read a Prettier parser from the minibuffer.

Returns a symbol identifying the parser (matching a known
Prettier parser name) or nil when nothing was selected."
  (let*
      ((parsers (prettier--parsers))
       (default (when parsers (symbol-name (car parsers))))
       (result
        (completing-read
         (if default
             (format
              "Prettier parser (%s): "
              default)
           "Prettier parser (infer): ")
         (mapcar (apply-partially #'nth 3)
                 (cdr (get 'prettier-enabled-parsers
                           'custom-type)))  ; collection
         nil                                ; predicate
         nil                                ; require-match
         nil                                ; initial
         'prettier-parser-history           ; history
         default)))                         ; default
    (when (> (length result) 0)
      (list (intern result)))))

(defun prettier--maybe-sync-config ()
  "Sync Prettier configuration in current buffer when appropriate.

Configuration is synced when `prettier-mode-sync-config-flag' is
non-nil and `prettier' is enabled in current buffer."
  (when (and prettier-mode
             prettier-mode-sync-config-flag)
    (with-temp-message "Prettier syncing config"
      (prettier--sync-config))))

(defun prettier--create-process (server-id node-command)
  "Create a new server process for SERVER-ID.

The process is a long-running server process that receives
requests, performs corresponding actions (such as formatting code
with Prettier) and returning a response.

SERVER-ID should be the symbol `local' for launching a local
process, or a remote identification as defined by `tramp-mode'
when launching a remote process.  Each process is started with
NODE-COMMAND.

The process is launched by running `node' with a minified version
of `bootstrap.js` as a script provided on the command line; this
then loads a minified version of `prettier-el.js' from stdin.

This setup is used for the following reasons:

- The script can't be loaded from the local file system because
  the process might be launched remotely.

- The whole script can't be put on the command line because doing
  so doesn't work reliably via `tramp'.

Additional considerations were:

- The payload sent via stdin is base64-encoded with line breaks
  to ensure it can be sent via `tramp'.

- The payload is minified and gzip-compressed to help with
  startup time via `tramp' on slow, non-compressed connections."
  (let* ((buf (get-buffer-create
               (format "*prettier %s*"
                       (or (prettier--buffer-remote-p 'host)
                           "(local)"))))
         (payload
          (with-temp-buffer
            (insert-file-contents-literally
             (concat prettier-el-home
                     "prettier-el.js.gz.base64"))
            (buffer-string)))
         (new-process
          (progn
            (with-current-buffer buf
              (erase-buffer)
              (setq buffer-undo-list t))
            (start-file-process
             "prettier"
             buf
             (prettier--pick-localname node-command)
             "--eval"
             (with-temp-buffer
               (insert-file-contents-literally
                (concat prettier-el-home "bootstrap-min.js"))
               (buffer-string))
             (number-to-string (length payload))))))
    (set-process-query-on-exit-flag new-process nil)
    (set-process-sentinel
     new-process
     (lambda (proc event)
       (unless (and (eq (process-status proc) 'signal)
                    (eq (process-exit-status proc) 3))
         (message "prettier-process (%s) quit unexpectedly: %s (%s)"
                  (process-get proc :server-id)
                  (string-trim event)
                  (buffer-string)))
       (unless prettier-keep-server-buffer-flag
         (kill-buffer (process-buffer proc)))
       (remhash node-command prettier-processes)))
    (set-process-coding-system new-process 'binary 'binary)
    (process-put new-process :server-id server-id)
    (condition-case nil
        (process-send-string new-process payload)
      (file-error
       (prettier--show-error
        "Cannot start prettier server on `%s': %s

%s"
        server-id
        (with-current-buffer (process-buffer new-process)
          (decode-coding-region (point-min) (point-max) 'utf-8 t))
        (prettier--startup-error-info server-id))))
    new-process))

(defun prettier--startup-error-info (server-id)
  "Return text explaining how to fix startup on host SERVER-ID.

SERVER-ID should be the symbol `local' for explaining issues with
a local process, or a remote identification as defined by
`tramp-mode' for a remote process."
  (if (eq server-id 'local)
      "Install Node or set `exec-path' so that it can be found.

Consider using package `exec-path-from-shell' or `nvm'."

    "Install Node on that host or set `tramp-remote-path' so that
it can be found."))

(defun prettier--get-process (&optional warmup-p)
  "Get or create a sub-process for the current buffer.

Non-nil WARMUP-P means that the process will be warmed up for the
current file.

If there is already a sub-process running on the host (local or
remote) corresponding to the current buffer, return that;
otherwise, launch a new one."
  (let* ((server-id (or (prettier--buffer-remote-p)
                        'local))
         (node-command (prettier--find-node server-id))
         (existing-process
          (gethash node-command prettier-processes))
         (existing-live-process
          (when (and existing-process
                     (process-live-p existing-process))
            existing-process)))
    (if (and existing-live-process (null warmup-p))
        existing-live-process
      (let ((start-time (current-time)))
        (prog1
            (let ((process
                   (or existing-live-process
                       (puthash
                        node-command
                        (prettier--create-process server-id
                                                  node-command)
                        prettier-processes))))
              (when warmup-p
                (prettier--request-iter
                 process
                 (list
                  "w"
                  (if prettier-editorconfig-flag "E" "e")
                  (prettier--local-file-name)
                  "\n\n")
                 t))
              process)
          (when prettier-show-benchmark-flag
            (prettier--delayed-message
             "Prettier process preparation for %s took %.1fms"
             (prettier--local-file-name)
             (* 1000 (float-time (time-subtract (current-time)
                                                start-time))))))))))

(defun prettier--delayed-message (string &rest objects)
  "Like `message', but delayed slightly.

STRING and OBJECTS are like for `format'."
  (run-at-time
   "1 sec"
   nil
   (lambda ()
     (apply #'message string objects))))

(defun prettier--show-error (string &rest objects)
  "Show STRING and OBJECTS as formatted error message.

The error is shown in a dedicated buffer that is shown in a
separate window."
  (let ((errbuf (get-buffer-create
                 prettier-error-buffer-name)))
    (with-current-buffer errbuf
      (setq buffer-read-only nil)
      (save-excursion
        (erase-buffer)
        (insert (ansi-color-apply
                 (apply #'format string objects))))
      (compilation-mode)
      (setq-local compilation-error-screen-columns nil)
      (display-buffer errbuf))))

(defun prettier--show-remote-error (filename error-msg)
  "Show the remote ERROR-MSG for FILENAME as appropriate."
  (cond
   ((seq-some
     (lambda (prefix)
       (string-prefix-p prefix error-msg))
     prettier-benign-errors)
    (prettier--delayed-message "%s" error-msg))
   ((and prettier-inline-errors-flag
         (string-match
          (concat "^" prettier-error-regex "$")
          error-msg))
    (let ((row (string-to-number (match-string 2 error-msg)))
          (column (string-to-number (match-string 3 error-msg)))
          (first-line (match-string 1 error-msg)))
      (when prettier-error-overlay
        (delete-overlay prettier-error-overlay))
      (save-excursion
        (widen)
        (goto-char (point-min))
        (forward-line row)
        (end-of-line)
        (let* ((eol (point))
               (ov (make-overlay eol (1+ eol)))
               (str (concat (when (eq eol (point-max)) "\n")
                            (make-string (1- column) 32)
                            "^ "
                            (propertize first-line
                                        'face
                                        'prettier-inline-error)
                            "\n")))
          (overlay-put ov 'phantom t)
          (overlay-put ov 'after-string str)
          (setq prettier-error-overlay ov)))))
   (t
    (prettier--show-error "%s: %s" filename error-msg))))

(defun prettier--clear-errors ()
  "Kill any error buffers and remove any overlays."
  (let ((errbuf (get-buffer prettier-error-buffer-name)))
    (when prettier-error-overlay
      (delete-overlay prettier-error-overlay))
    (when errbuf
      (let ((win (get-buffer-window errbuf)))
        (if win
            (quit-window t win)
          (kill-buffer errbuf))))))

(defun prettier--sync-config ()
  "Try to sync prettier configuration for current buffer.

Tries loading the configuration, ignoring failure with a message.

If loaded successfully, uses it to set a variety of buffer-local
variables in an effort to make pre-formatting indentation etc as
close to post-formatting as possible."
  (condition-case-unless-debug err
      (let* ((config (prettier--load-config))
             (options (plist-get config :options)))
        (setq
         prettier-last-parser
         (plist-get config :bestParser)

         prettier-version
         (plist-get (plist-get config :versions) :prettier)

         prettier-previous-local-settings
         (when options
           (seq-filter
            #'identity
            (apply
             #'append
             (mapcar
              (lambda (setting)
                (let* ((vars (nth 0 setting))
                       (source (nth 1 setting))
                       (value (funcall
                               (or (nth 2 setting) #'identity)
                               (if (keywordp source)
                                   (plist-get options source)
                                 source))))
                  (unless (eq value 'unchanged)
                    (mapcar
                     (lambda (var)
                       (when (boundp var)
                         (let ((result
                                (list var (local-variable-p var)
                                      (eval var)
                                      value)))
                           (set (make-local-variable var) value)
                           result)))
                     vars))))
              prettier-sync-settings))))))
    ;; Ignore any errors but print a warning
    ((debug error)
     (message "Could not sync Prettier config, consider setting \
`prettier-mode-sync-config-flag' to nil: %S" err))))

(defun prettier--revert-synced-config ()
  "Revert any change made by `prettier--sync-config'."
  (mapc
   (lambda (local-setting)
     (let ((var (car local-setting)))
       (cond
        ((or (not (boundp var))
             (not (eq (eval var) (nth 3 local-setting))))
         ;; If the setting has changed since we set it, or the
         ;; variable was unbound in the meantime, leave it alone
         nil)
        ((null (nth 1 local-setting))
         ;; setting wasn't local before, kill the variable
         (kill-local-variable var))
        (t
         ;; otherwise, set it to previous value
         (set var (nth 2 local-setting))))))
   prettier-previous-local-settings))

(iter2-defun prettier--request-iter (prettier-process
                                     request
                                     &optional fire-and-forget-p)
  "Send REQUEST to PRETTIER-PROCESS, yield commands."
  (condition-case err
      (let (p (buf (process-buffer prettier-process)))
        (process-send-string prettier-process
                             (apply #'concat request))
        (unless fire-and-forget-p
          (unwind-protect
              (catch 'end-of-message
                (with-current-buffer buf
                  (while t
                    (while
                        (null
                         (save-excursion
                           (goto-char (or p (point-min)))
                           (and
                            (or p (setq p (re-search-forward
                                           "#prettier\.el-sync#\n" nil t)))
                            (looking-at
                             "\\([DEIMOPTVZ]\\)\\([[:xdigit:]]+\\)\n")
                            (let* ((m (match-end 0))
                                   (kind (string-to-char
                                          (match-string 1)))
                                   (len (string-to-number
                                         (match-string 2)
                                         16)))
                              (cond
                               ((eq kind ?Z)
                                (setq p m)
                                (throw 'end-of-message nil))
                               ((member kind '(?I ?O ?E ?V ?P))
                                (when (<= (+ m len) (point-max))
                                  (iter-yield
                                   (cons kind (list m (+ m len))))
                                  (setq p (+ m len 1))))
                               (t (setq p m)
                                  (iter-yield (cons kind len))
                                  t))))))
                      (let ((len (point-max)))
                        (and
                         (null (tramp-accept-process-output
                                prettier-process
                                prettier-timeout-seconds))
                         (eq len (point-max))
                         (error  "Prettier timed out after %s seconds"
                                 prettier-timeout-seconds)))
                      (when (eq (process-status prettier-process)
                                'exit)
                        (error "Node sub-process died"))))))
            (when p
              (with-current-buffer buf
                (delete-region 1 p))))))
    ((quit error)
     ;; FIXME: need more efficient recovery from quit
     (quit-process prettier-process)
     (signal (car err) (cdr err)))))

(defun prettier--load-config ()
  "Load prettier configuration for current buffer."
  (let* ((start-time (current-time))
         (prettier-process (prettier--get-process))
         (process-buf (process-buffer prettier-process))
         (parsers (prettier--parsers))
         (iter (prettier--request-iter
                prettier-process
                (list "o"
                      (if prettier-editorconfig-flag "E" "e")
                      (if prettier-infer-parser-flag "I" "i")
                      (prettier--local-file-name)
                      "\n" (if parsers (string-join
                                        (mapcar #'symbol-name parsers)
                                        ",")
                             "-")
                      "\n\n")))
         config)
    (unwind-protect
        (progn
          (iter-do (command iter)
            (if (eq (car command) ?O)
                (let* ((json-object-type 'plist)
                       (json-false nil))
                  (setq
                   config
                   (json-read-from-string
                    (base64-decode-string
                     (with-current-buffer process-buf
                       (buffer-substring-no-properties
                        (nth 1 command)
                        (nth 2 command)))))))))
          (when prettier-show-benchmark-flag
            (message
             "Prettier load-config took %.1fms"
             (* 1000
                (float-time (time-subtract (current-time)
                                           start-time)))))
          config)
      (iter-close iter))))

(defun prettier--default-callback (message process-buffer)
  "Default response callback.

Handles any MESSAGE only by handling errors.

PROCESS-BUFFER is the process buffer."
  (dolist (command message)
    (let ((kind (car command)))
      (cond

       ((eq kind ?E)
        (prettier--show-remote-error
         "Internal"
         (with-temp-buffer
           (insert (with-current-buffer process-buffer
                     (buffer-substring-no-properties
                      (nth 0 (cdr command))
                      (nth 1 (cdr command)))))
           (base64-decode-region 1 (point))
           (buffer-string))))))))

(defun prettier--maybe-show-benchmark (start-time end-time timestamps)
  "If so configured, show timing information for the last operation.

START-TIME and END-TIME give the total time of the operation, and
TIMESTAMPS is additional information received from the server."
  (when prettier-show-benchmark-flag
    (let ((total (float-time (time-subtract end-time start-time)))
          (prettier (- (nth 1 timestamps)
                       (nth 0 timestamps))))
      (prettier--delayed-message
       "Prettier format took %.1fms + %.1fms"
       (* prettier 1000)
       (* (- total prettier) 1000)))))

(defun prettier--format-iter (parsers
                              filename
                              tempfile)
  "Launch format operation remotely and return iterator.

PARSERS are the enabled parsers, FILENAME is the original
filename and TEMPFILE is where the buffer contents before
formatting are stored."
  (prettier--request-iter
   (prettier--get-process)
   (list
    "f"
    (if prettier-editorconfig-flag "E" "e")
    (if prettier-infer-parser-flag "I" "i")
    filename
    "\n" (if parsers
             (string-join
              (mapcar #'symbol-name parsers)
              ",")
           "-")
    "\n" (prettier--pick-localname tempfile)
    "\n\n")))

(defun prettier--payload (process-buffer command)
  "Return the payload string for the given COMMAND.

PROCESS-BUFFER is the process buffer in which the command was
received."
  (let ((range (cdr command)))
    (base64-decode-string
     (with-current-buffer process-buffer
       (buffer-substring-no-properties (nth 0 range)
                                       (nth 1 range))))))

(defun prettier--prettify (&optional
                           parsers
                           start
                           end)
  "Format the current buffer from START to END.

The first supported parser in PARSERS will be used for
formatting."
  (let* ((start-time (current-time))
         (process-buf (process-buffer (prettier--get-process)))
         (start-point (copy-marker (or start (point-min)) nil))
         (end-point (copy-marker (or end (point-max)) t))
         (point-before (copy-marker (point)))
         (filename (prettier--local-file-name))
         (tempfile (make-nearby-temp-file "prettier-emacs."))
         result-point
         any-errors
         timestamps
         (buffer-undo-list-backup buffer-undo-list)
         (iter (prettier--format-iter parsers
                                      filename
                                      tempfile)))
    (unwind-protect
        (when (< start-point end-point)
          (let ((write-region-inhibit-fsync t)
                (coding-system-for-write 'utf-8-unix))
            (write-region start-point
                          end-point
                          tempfile
                          nil 'no-visit nil nil))
          (condition-case err
              (save-excursion
                (widen)
                (goto-char start-point)

                (iter-do (command iter)
                  (let ((kind (car command))
                        (command-str
                         (lambda ()
                           (prettier--payload process-buf command))))
                    (cond
                     ((eq kind ?M)
                      (forward-char (cdr command)))

                     ((eq kind ?I)
                      (insert (funcall command-str)))

                     ((eq kind ?D)
                      (delete-region (point)
                                     (+ (point) (cdr command))))

                     ((eq kind ?T)
                      (push (/ (cdr command) 1000.0) timestamps))

                     ((eq kind ?E)
                      (setq any-errors t)
                      (prettier--show-remote-error
                       filename
                       (prettier--payload process-buf command)))

                     ((eq kind ?P)
                      (when (and (null start) (null end))
                        (setq prettier-last-parser
                              (funcall command-str))))

                     ((eq kind ?V)
                      (setq prettier-version (funcall command-str)))))))
            ((quit error)
             (ignore-errors
               (setq any-errors t)
               (let ((buffer-undo-list t))
                 (erase-buffer)
                 (let ((coding-system-for-read 'utf-8-unix))
                   (insert-file-contents tempfile))
                 (setq buffer-undo-list buffer-undo-list-backup)
                 (goto-char point-before)))
             (signal (car err) (cdr err))))

          (unless any-errors
            (prettier--clear-errors)
            (when result-point (goto-char (1+ result-point))))

          (prettier--maybe-show-benchmark start-time
                                          (current-time)
                                          timestamps))
      (iter-close iter)
      (delete-file tempfile))))

(defun prettier--node-from-nvm ()
  "Find the best `node' executable with `nvm'.

The result is the path to the `node' executable in the highest
install stable (even) node version, with minimum major version 6,
or nil when no node versions matching this criteria are installed
via nvm or when nvm itself is not installed."
  (ignore-errors
    (when (fboundp #'nvm--installed-versions)
      (let* ((versions
              (mapcar
               (lambda (version)
                 (cons (mapcar
                        #'string-to-number
                        (cdr (split-string (car version) "[v.]+")))
                       (cdr version)))
               (nvm--installed-versions)))
             (node-dir
              (cadar
               (last
                (cl-sort
                 (seq-filter
                  (lambda (v)
                    (let ((major (caar v)))
                      (and (zerop (% major 2)) (>= major 6))))
                  versions)
                 #'version-list-< :key #'car))))
             (node-command
              (when node-dir
                (concat (file-name-as-directory node-dir)
                        "bin/node"))))
        (when (file-executable-p node-command)
          node-command)))))

(defun prettier--find-node (server-id)
  "Find the name or path of the node executable to use.

SERVER-ID gives the context.  It is the symbol `local' when the
path of the node executable on the local host is sought,
otherwise the remote identification as defined by `tramp-mode'."
  (if (eq server-id 'local)
      (or prettier-nvm-node-command-cache
          (let ((nvm-node-command (prettier--node-from-nvm)))
            (when nvm-node-command
              (setq prettier-nvm-node-command-cache nvm-node-command)
              nvm-node-command))
          (executable-find "node")
          "node")
    ;; Remote
    (concat server-id "node")))

(defun prettier--pick-localname (file)
  "Return the local name or path from FILE."
  (or (and (tramp-tramp-file-p file)
           (save-match-data
             (and (string-match tramp-file-name-regexp file)
                  (match-string (nth 4 tramp-file-name-structure)
                                file))))
      file))

(defun prettier--parsers ()
  "Return an alist of parsers to use for the current buffer.

The return value is the first enabled parser that is configured
to be used with the current major mode (or its nearest parent,
for derived modes.)"
  (or prettier-parsers
      (seq-filter
       (lambda (parser)
         (member parser prettier-enabled-parsers))
       (prettier--parsers-for-mode
        (or (and (fboundp 'mhtml-mode)
                 (get-text-property (point) 'mhtml-submode)
                 'mhtml-mode)
            (and (fboundp 'svelte-mode)
                 (get-text-property (point) 'svelte-submode)
                 'svelte-mode)
            (and (boundp 'mmm-primary-mode)
                 mmm-primary-mode)
            major-mode)))))

(defun prettier--parsers-for-mode (mode)
  "Return a list of parsers for the given major MODE.

The mode's parents are searched recursively when there are no
parsers configured for it and it is a derived mode."
  (when mode
    (or
     (let ((major-mode-parsers
            (or (when (eq mode 'web-mode)
                  (cdr (assoc
                        web-mode-content-type
                        prettier-web-mode-content-type-parsers)))
                (cdr (assoc mode
                            prettier-major-mode-parsers)))))
       (if (functionp major-mode-parsers)
           (funcall major-mode-parsers)
         major-mode-parsers))
     (prettier--parsers-for-mode
      (get mode 'derived-mode-parent)))))

(defun prettier--local-file-name ()
  "Return the buffer's local filename."
  (or (prettier--buffer-remote-p 'localname)
      buffer-file-name
      (concat "(unsaved)" (buffer-name))))


;;;; Integration with other packages

(add-to-list 'compilation-error-regexp-alist-alist
             (cons 'prettier prettier-compilation-regexps))
(add-to-list 'compilation-error-regexp-alist 'prettier)


;;;; Ensure we're running latest JavaScript sub-process

;; (On initial load, `prettier-restart' is a no-op.)
(when load-in-progress
  (prettier-restart))


;;;; Footer

(provide 'prettier)

;; LocalWords: editorconfig minibuffer minified nvm parsers stdin npm

;;; prettier.el ends here
