;;; prettier-benchmarks.el --- Benchmarks for prettier.el  -*- lexical-binding: t; -*-

;; Copyright (c) 2022-present Julian Scheid

;;; Commentary:

;; Benchmark suite for prettier.el.

;; Run with: ./makem.sh benchmark

;;; Code:



(require 'prettier)

(setq prettier-el-home (concat
                        (file-name-directory load-file-name)
                        "dist/"))

(defun prettier-benchmark ()
  "Run all benchmarks."
  (prettier-run-benchmark "small-file-no-edits.js")
  (prettier-run-benchmark "small-file-many-edits.js")
  (prettier-run-benchmark "large-file-no-edits.js")
  (prettier-run-benchmark "large-file-many-edits.js"))

(defun prettier-run (file contents)
  "Run Prettier on FILE with CONTENTS."
  (with-temp-buffer
    (insert contents)
    (setq buffer-file-name file)
    (rename-buffer file)
    (set-auto-mode)
    (prettier-prettify)
    (let* ((error-buffer (get-buffer prettier-error-buffer-name))
           (errors (if error-buffer (with-current-buffer error-buffer (buffer-string)) "")))
      (unless (equal "" errors)
        (error "Errors: %s" errors)))))

(defun prettier-run-benchmark (name)
  "Run benchmarks for file named NAME."
  (let* ((file (file-truename (concat "benchmark-cases/" name)))
         (contents
          (with-temp-buffer
            (insert-file-contents file)
            (buffer-string))))
    (dotimes (_ 100) (prettier-run file contents))
    (let ((num-runs 100))
      (princ
       (format
        "%s: %dms per run\n"
        name
        (round
         (*
          1000.0
          (/
           (car
            (benchmark-run
                num-runs
              (prettier-run file contents)))
           num-runs))))
       #'external-debugging-output))))

(provide 'prettier-benchmarks)

;;; prettier-benchmarks.el ends here
