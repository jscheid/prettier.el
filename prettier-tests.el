;;; prettier-tests.el --- Tests for prettier.el  -*- lexical-binding: t; -*-

;; Copyright (c) 2018-present Julian Scheid

;; Package-Requires: ((markdown-mode "20230723")(web-mode "20200501") (elm-mode "20200406") (pug-mode "20180513") (svelte-mode "20210222") (toml-mode "20161107") (solidity-mode "20200418") (vue-mode "20190415") (lsp-mode "20201111") (noflet "20141102") (f "20191110") (json-mode "20190123") (typescript-mode "20201002") (ert-async "0.1") (f "20220531"))

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
  "Run prettier formatting test in DIRECTORY."
  (let ((default-directory directory))
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
   (let ((dirname (file-name-base test-directory))
         (pdirname (file-name-base
                    (directory-file-name
                     (file-name-directory test-directory)))))
     (eval
      `(ert-deftest
           ,(intern (format "prettier-testcase-%s/%s" dirname pdirname)) ()
         ,(format "Run testcases %s in %s." dirname pdirname)
         (condition-case err
             (prettier--run-test-case ,test-directory)
           ((quit error)
            (sit-for 0.25)
            (signal (car err) (cdr err))))))))
 (mapcar
  #'car
  (seq-filter
   (lambda (file-and-attributes)
     (and
      (not (equal "node_modules" (file-name-base (car file-and-attributes))))
      (eq t (cadr file-and-attributes))))
   (apply
    'append
    (mapcar
     (lambda (x)
       (directory-files-and-attributes
        (file-name-as-directory (concat default-directory x))
        t
        "[a-z].*"))
     '("test-stable" "test-v2"))))))


(defmacro prettier--ert-deftest-for-dirs (testname testdoc dirnames &rest body)
  "Execute the specified test code in multiple directories.
TESTNAME and TESTDOC are used as macro name and doc string for `ert-deftest',
respectively.  DIRNAMES is a list of directories in which to run the test code.
BODY is the test code."
  `(mapc
    (lambda (dirname)
      (eval
       (list
        'ert-deftest
        (intern (format "%s/%s" ,testname dirname))
        '()
        (format "%s Runs in %s." ,testdoc dirname)
        (list 'cd (list 'concat 'default-directory dirname))
        '(progn ,@body))))
    ,dirnames))

(defmacro prettier--ert-deftest-async-for-dirs (testname testdoc dirnames body)
  "Async version of `prettier--ert-deftest-for-dirs'.
TESTNAME, TESTDOC and DIRNAMES are the same but BODY must be a closure that
accepts a `done' argument."
  `(mapc
    (lambda (dirname)
      (eval
       (list
        'ert-deftest-async
        (intern (format "%s/%s" ,testname dirname))
        '(done)
        (format "%s Runs in %s." ,testdoc dirname)
        (list 'cd (list 'concat 'default-directory dirname))
        '(funcall ,body done))))
    ,dirnames))


(prettier--ert-deftest-for-dirs
 "prettier-test-prettier-version"
 "Ensure the prettier version of each directory."
 '("test-stable" "test-v2")
 (with-temp-buffer
   (setq buffer-file-name (concat default-directory "test.js"))
   (js-mode)
   (prettier-info)
   (let ((dirname (file-name-base (directory-file-name default-directory))))
     (with-current-buffer (get-buffer "prettier-info.el")
       (let* ((info (read (buffer-substring (point-min) (point-max))))
              (vers (plist-get (plist-get info :prettier-options) :versions))
              (pver (plist-get vers :prettier)))
         (cond
          ((equal dirname "test-stable") (should (equal pver "3.0.0")))
          ((equal dirname "test-v2") (should (equal pver "2.6.2")))
          (t (error (format "Unexpected directory name: %s" dirname)))))))))


(prettier--ert-deftest-for-dirs
 "web-mode-typescript"
 ""
 '("test-stable" "test-v2")
 (with-temp-buffer
   (setq buffer-file-name (concat default-directory "test.ts"))
   (web-mode)
   (should (equal (prettier--parsers) '(typescript babel-ts)))))


(prettier--ert-deftest-for-dirs
 "prettier--parsers-temp-buffer"
 ""
 '("test-stable" "test-v2")
 (with-temp-buffer
   (setq buffer-file-name (concat default-directory "test.js"))
   (js-mode)
   (require 'lsp-mode)
   (should (equal (prettier--parsers)
                  '(babel meriyah espree flow babel-flow)))))


(prettier--ert-deftest-for-dirs
 "customize-prettier"
 ""
 '("test-stable" "test-v2")
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

(prettier--ert-deftest-async-for-dirs
 "restart-prettier"
 ""
 '("test-stable" "test-v2")
 '(lambda (done)
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
               (funcall done))))))))))


(prettier--ert-deftest-for-dirs
 "load-local-config-once"
 "Ensure config is loaded only once for buffer without file."
 '("test-stable" "test-v2")
 (with-temp-buffer (js-mode) (prettier-mode))
 (cl-flet ((prettier--load-config (&rest) (error "Called again")))
   (with-temp-buffer (js-mode) (prettier-mode))))


(prettier--ert-deftest-for-dirs
 "org-mode-src-block"
 "Formatting a source block in org mode."
 '("test-stable" "test-v2")
 (with-temp-buffer
   (org-mode)
   (setq buffer-file-name (concat default-directory "test.org"))
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


(prettier--ert-deftest-for-dirs
 "sync-test"
 "Test syncing settings from Prettier."
 '("test-stable" "test-v2")
 (with-temp-buffer
   (js-mode)
   (setq buffer-file-name (concat default-directory "test.js"))
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
