;;; create-pkg.el --- Generate prettier.pkg.el  -*- lexical-binding: t; -*-

;;; Commentary:

(require 'package)
(require 'lisp-mnt)

;;; Code:

(with-temp-buffer
  (insert-file-contents "prettier.el")
  (let ((pkg-desc (package-buffer-info)))
    (push (cons :keywords
                (split-string (lm-header "keywords") "[ ,]+"))
          (package-desc-extras pkg-desc))
    (package-generate-description-file
     pkg-desc
     "prettier-pkg.el")))

;;; create-pkg-el.el ends here
