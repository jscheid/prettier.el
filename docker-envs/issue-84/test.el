#!/usr/bin/env -S emacs --script

;; ---------------------------------------------------------------------
;; Main test script. Open `test.js` on remote server, run Prettier on
;; it and print output
;; ---------------------------------------------------------------------

;; Give the other container some time to start up
(let ((wait-seconds 3))
  (message "Waiting %d seconds for other container to start..."
           wait-seconds)
  (sleep-for wait-seconds))

;; Ensure Prettier is loaded
(package-initialize)

;; Open our test file via SSH
(find-file "/ssh:remote:test.js")

;; Prettify it
(prettier-prettify)

;; Print the result
(let ((divider (concat (make-string 72 ?-) "\n")))
  (princ (concat divider (buffer-string) divider)))


# Local Variables:
# mode: Lisp
# End:
