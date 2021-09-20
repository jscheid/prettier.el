;;; prettier-tests.el --- Tests for prettier.el  -*- lexical-binding: t; -*-

;; Copyright (c) 2018-present Julian Scheid

;; Package-Requires: ((web-mode "20200501") (elm-mode "20200406") (pug-mode "20180513") (svelte-mode "20210222") (toml-mode "20161107") (solidity-mode "20200418") (vue-mode "20190415") (lsp-mode "20201111") (noflet "20141102") (f "20191110") (json-mode "20190123") (typescript-mode "20201002") (ert-async "0.1"))

;;; Commentary:

;; Test suite for prettier.el.

;;; Code:



(require 'ert)
(require 'noflet)
(require 'thingatpt)
(require 'prettier)
(require 'svelte-mode)
(require 'typescript-mode)

(eval-when-compile
  (require 'ert-async))

(setq prettier-el-home (concat
                        (file-name-directory load-file-name)
                        "dist/"))

(defun prettier--eval-file-if-exists (filename)
  "Read and eval file FILENAME if it exists."
  (let ((setup-elisp filename))
    (when (file-exists-p setup-elisp)
      (eval
       (thing-at-point--read-from-whole-string
        (f-read-text setup-elisp))))))

(defun prettier--run-test-case (directory)
  "Run prettier test in DIRECTORY."
  (let ((default-directory directory))
    (shell-command "npm install")
    (mapc
     (lambda (original-file)
       (let ((actual
              (with-temp-buffer
                (insert-file-contents original-file)
                (setq buffer-file-name original-file)
                (rename-buffer original-file)
                (prettier--eval-file-if-exists "prepare.elisp")
                (set-auto-mode)
                (prettier--eval-file-if-exists "setup.elisp")
                (prettier-prettify)
                (prettier--eval-file-if-exists "test.elisp")
                (buffer-substring-no-properties (point-min)
                                                (point-max))))
             (expected
              (with-temp-buffer
                (insert-file-contents
                 (replace-regexp-in-string
                  "\\.original\\."
                  ".prettier."
                  original-file))
                (buffer-string))))
         (should (equal actual expected))
         (prettier--eval-file-if-exists "teardown.elisp")))
     (directory-files directory t "\\.original\\."))))

(mapc
 (lambda (test-directory)
   (eval
    `(ert-deftest
         ,(intern (format "prettier-test-%s" (file-name-base test-directory))) ()
       ,(format "Run tests in %S." test-directory)
       (condition-case err
           (prettier--run-test-case ,test-directory)
         ((quit error)
          (sit-for 0.25)
          (signal (car err) (cdr err)))))))
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
    (should (equal (prettier--parsers) '(typescript babel-ts)))))

(ert-deftest prettier--parsers-temp-buffer ()
  (with-temp-buffer
    (js-mode)
    (require 'lsp-mode)
    (should (equal (prettier--parsers) '(babel meriyah espree flow babel-flow)))))

(ert-deftest customize-prettier ()
  (customize-option 'prettier-pre-warm)
  (customize-option 'prettier-inline-errors-flag)
  (customize-option 'prettier-mode-sync-config-flag)
  (customize-option 'prettier-editorconfig-flag)
  (customize-option 'prettier-infer-parser-flag)
  (customize-option 'prettier-enabled-parsers)
  (customize-option 'prettier-mode-ignore-buffer-function)
  (customize-option 'prettier-lighter))

(defun prettier-process-p (process)
  "Return non-nil if PROCESS is like a Prettier process."
  (and (string-match "^prettier" (process-name process))
       (process-get process :server-id)
       (process-live-p process)))

(defun any-prettier-process-p ()
  "Return non-nil if any process is like a Prettier process."
  (seq-some #'prettier-process-p (process-list)))

(defun delay (callback)
  "Call CALLBACK delayed by a little while."
  (run-at-time 0.5 nil callback))

(ert-deftest-async restart-prettier (done)
  (prettier--quit-all-processes)
  (delay
   (lambda ()
     (with-current-buffer (get-buffer-create "test.js")
       (js-mode)
       (should (not (any-prettier-process-p)))
       (prettier-mode)
       (delay
        (lambda ()
          (should (any-prettier-process-p))
          (prettier-restart)
          (delay
           (lambda ()
             (should (any-prettier-process-p))
             (funcall done)))))))))

(provide 'prettier-tests)

;;; prettier-tests.el ends here
