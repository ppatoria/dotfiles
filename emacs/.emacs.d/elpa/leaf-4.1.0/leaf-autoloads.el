;;; leaf-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from leaf.el

(autoload 'leaf-available-keywords "leaf" "\
Return current available `leaf' keywords list." t)
(autoload 'leaf-pp-to-string "leaf" "\
Return format string of `leaf' SEXP like `pp-to-string'.

(fn SEXP)" nil t)
(autoload 'leaf-pp "leaf" "\
Output the pretty-printed representation of leaf SEXP.

(fn SEXP)")
(autoload 'leaf-create-issue-template "leaf" "\
Create issue template buffer." t)
(autoload 'leaf-expand "leaf" "\
Expand `leaf' at point." t)
(autoload 'leaf-key-describe-bindings "leaf" "\
Display all the bindings configured via `leaf-key'." t)
(autoload 'leaf "leaf" "\
Symplify your `.emacs' configuration for package NAME with ARGS.

(fn NAME &rest ARGS)" nil t)
(function-put 'leaf 'lisp-indent-function 'defun)
(register-definition-prefixes "leaf" '("leaf-"))

;;; End of scraped data

(provide 'leaf-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; leaf-autoloads.el ends here