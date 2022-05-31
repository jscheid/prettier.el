;;; create-pkg.el --- Generate prettier.pkg.el  -*- lexical-binding: t; -*-

;;; Commentary:

(require 'package)
(require 'lisp-mnt)

;;; Code:

(defun prettier--create-pkg-el (output-filename)
  "Write `prettier-pkg.el' to OUTPUT-FILENAME."
  (with-temp-buffer
    (insert-file-contents "prettier.el")
    (let ((pkg-desc (package-buffer-info)))
      (let ((version-suffixes (getenv "VERSION_SUFFIXES")))
        (when version-suffixes
          (setf (package-desc-version pkg-desc)
                (append (package-desc-version pkg-desc)
                        (mapcar #'string-to-number
                                (split-string version-suffixes
                                              "\\."))))))
      (push (cons :keywords
                  (split-string (lm-header "keywords") "[ ,]+"))
            (package-desc-extras pkg-desc))
      (package-generate-description-file
       pkg-desc
       output-filename))))

;;; create-pkg-el.el ends here
