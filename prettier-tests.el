;;; prettier-tests.el --- Tests for prettier.el  -*- lexical-binding: t; -*-

;; Copyright (c) 2018-present Julian Scheid

;; Package-Requires: ((web-mode "20200501") (elm-mode "20200406") (pug-mode "20180513") (svelte-mode "20210222") (toml-mode "20161107") (solidity-mode "20200418") (vue-mode "20190415") (lsp-mode "20201111") (noflet "20141102") (f "20191110") (json-mode "20190123") (typescript-mode "20201002") (ert-async "0.1") (f "20220531"))

;;; Commentary:

;; Test suite for prettier.el.

;;; Code:



(require 'ert)
(require 'noflet)
(require 'thingatpt)
(require 'prettier)
(require 'svelte-mode)
(require 'typescript-mode)
(require 'f)
(require 'hexl)

(eval-when-compile
  (require 'ert-async))

(defun prettier--hexlify-str (str)
  "Return STR, hexlified."
  (with-temp-buffer
    (insert str)
    (hexlify-buffer)
    (buffer-string)))

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
    (shell-command "yarn")
    (mapc
     (lambda (original-file)
       (let ((actual
              (let ((prettier-diff-timeout-seconds 0)
                    (out-file (replace-regexp-in-string
                               "\\.original\\."
                               ".test."
                               original-file)))
                (with-temp-buffer
                  (unwind-protect
                      (progn
                        (insert-file-contents original-file)
                        (setq buffer-file-name out-file)
                        (rename-buffer out-file)
                        (prettier--eval-file-if-exists "prepare.elisp")
                        (set-auto-mode)
                        (prettier--eval-file-if-exists "setup.elisp")
                        (prettier-mode 1)
                        (save-buffer 0)
                        (prettier--eval-file-if-exists "test.elisp")
                        (should (equal (with-current-buffer "*prettier (local)*"
                                         (buffer-string))
                                       ""))
                        (let ((error-buffer
                               (get-buffer prettier-error-buffer-name)))
                          (when error-buffer
                            (should (equal (with-current-buffer error-buffer
                                             (buffer-string))
                                           ""))))

                        (f-read-bytes out-file))
                    (delete-file out-file)))))
             (expected
              (f-read-bytes
               (replace-regexp-in-string
                "\\.original\\."
                ".prettier."
                original-file))))
         (should (equal (prettier--hexlify-str actual)
                        (prettier--hexlify-str expected)))
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

(ert-deftest load-local-config-once ()
  "Ensure config is loaded only once for buffer without file."
  (with-temp-buffer (js-mode) (prettier-mode))
  (cl-flet ((prettier--load-config (&rest) (error "Called again")))
    (with-temp-buffer (js-mode) (prettier-mode))))

(ert-deftest org-mode-src-block ()
  "Formatting a source block in org mode."
  (with-temp-buffer
    (org-mode)
    (setq buffer-file-name
          (concat
           prettier-el-home
           "test.org"))
    (insert "\
Header

#+begin_src js
  if ( true ) {
    console.log('Hello, World!');
  }
#+end_src

Footer")
    (goto-char 24)
    (prettier-prettify-org-src-code-at-point)
    (should (equal (buffer-string)
                   "\
Header

#+begin_src js
  if (true) {
    console.log(\"Hello, World!\");
  }
#+end_src

Footer"))))

(ert-deftest sync-test ()
  "Test syncing settings from Prettier."
  (with-temp-buffer
    (js-mode)
    (setq buffer-file-name (concat prettier-el-home "test.js"))

    (setq js-indent-level 10)
    (prettier-mode)
    (should (equal js-indent-level 2))
    (should (local-variable-p 'js-indent-first-init))
    (should (null js-indent-first-init))
    (prettier-mode -1)
    (should (equal js-indent-level 10))
    (should (not (local-variable-p 'js-indent-first-init)))))

(provide 'prettier-tests)

;;; prettier-tests.el ends here
