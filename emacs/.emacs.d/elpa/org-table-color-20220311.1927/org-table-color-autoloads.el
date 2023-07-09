;;; org-table-color-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from org-table-color.el

(autoload 'org-table-color "org-table-color" "\
Color the 'org-mode' table at 'point', given a GET-FACE function.

GET-FACE must accept a single numerical argument (the value of
the cell) and return either a plist representing a face or nil.
When nil, no styling of that cell will occur. Further, no styling
will occur if the cell value is not a number.

See `org-table-color--color-by-correlation' for an example.

(fn GET-FACE)")
(autoload 'org-table-color-correlation-matrix "org-table-color" "\
Color the 'org-mode' table at 'point' that represents a Correlation Matrix." t)
(register-definition-prefixes "org-table-color" '("org-table-color--color-"))

;;; End of scraped data

(provide 'org-table-color-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; org-table-color-autoloads.el ends here
