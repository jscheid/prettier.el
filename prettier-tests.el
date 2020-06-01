;;; prettier-tests.el --- Tests for prettier.el  -*- lexical-binding: t; -*-

;; Copyright (c) 2018-present Julian Scheid

;; Package-Requires: ((web-mode "20200501") (elm-mode "20200406") (pug-mode "20180513") (svelte-mode "20200327") (toml-mode "20161107") (solidity-mode "20200418"))

;;; Commentary:

;; Test suite for prettier.el.

;;; Code:



(require 'ert)
(require 'prettier)

(defun prettier--run-test-case (directory)
  "Run prettier test in DIRECTORY."
  (let ((default-directory directory))
    (shell-command "npm install"))
  (mapc
   (lambda (original-file)
     (let ((actual
            (with-temp-buffer
              (insert-file-contents original-file)
              (setq buffer-file-name original-file)
              (set-auto-mode)
              (prettier-prettify)
              (buffer-string)))
           (expected
            (with-temp-buffer
              (insert-file-contents
               (replace-regexp-in-string
                "\\.original\\."
                ".prettier."
                original-file))
              (buffer-string))))
       (should (equal actual expected))))
   (directory-files directory t "\\.original\\.")))

(mapc
 (lambda (test-directory)
   (eval
    `(ert-deftest
         ,(intern (format "prettier-test-%s" (file-name-base test-directory))) ()
       ,(format "Run tests in %S." test-directory)
       (prettier--run-test-case ,test-directory))))
 (mapcar
  #'car
  (seq-filter
   (lambda (file-and-attributes)
     (eq t (cadr file-and-attributes)))
   (directory-files-and-attributes
    (concat default-directory
            "test-cases/")
    t
    "[a-z].*"))))

(ert-deftest web-mode-typescript ()
  (with-temp-buffer
    (setq buffer-file-name "test.ts")
    (web-mode)
    (should (equal (prettier--parsers) '(typescript)))))

(ert-deftest customize-prettier ()
  (customize-option 'prettier-pre-warm)
  (customize-option 'prettier-inline-errors-flag)
  (customize-option 'prettier-mode-sync-config-flag)
  (customize-option 'prettier-editorconfig-flag)
  (customize-option 'prettier-infer-parser-flag)
  (customize-option 'prettier-enabled-parsers)
  (customize-option 'prettier-mode-ignore-buffer-function)
  (customize-option 'prettier-lighter))

(provide 'prettier-tests)

;;; prettier-tests.el ends here
