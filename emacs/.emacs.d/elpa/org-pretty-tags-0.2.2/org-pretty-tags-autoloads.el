;;; org-pretty-tags-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from org-pretty-tags.el

(defvar org-pretty-tags-surrogate-strings '(("imp" . "☆") ("idea" . "💡") ("money" . "$$$") ("easy" . "₰") ("music" . "♬")) "\
List of pairs of tag and replacement e.g. (\"money\" . \"$$$\") of
  surrogates for tags.")
(custom-autoload 'org-pretty-tags-surrogate-strings "org-pretty-tags" t)
(defvar org-pretty-tags-surrogate-images 'nil "\
List of pairs of tag and file-path to an image e.g. (\"@alice\" . \"/images/alice.png\") of
  image surrogates for tags.")
(custom-autoload 'org-pretty-tags-surrogate-images "org-pretty-tags" t)
(defvar org-pretty-tags-mode-lighter " pretty-tags" "\
Text in the mode line to indicate that the mode is on.")
(custom-autoload 'org-pretty-tags-mode-lighter "org-pretty-tags" t)
(autoload 'org-pretty-tags-mode "org-pretty-tags" "\
Display surrogates for tags in buffer.

This mode is local to Org mode buffers.

Special: when invoked from an Org agenda buffer the mode gets
applied to every Org mode buffer.

This is a minor mode.  If called interactively, toggle the
`Org-Pretty-Tags mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `org-pretty-tags-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t)
(put 'org-pretty-tags-global-mode 'globalized-minor-mode t)
(defvar org-pretty-tags-global-mode nil "\
Non-nil if Org-Pretty-Tags-Global mode is enabled.
See the `org-pretty-tags-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `org-pretty-tags-global-mode'.")
(custom-autoload 'org-pretty-tags-global-mode "org-pretty-tags" nil)
(autoload 'org-pretty-tags-global-mode "org-pretty-tags" "\
Toggle Org-Pretty-Tags mode in all buffers.
With prefix ARG, enable Org-Pretty-Tags-Global mode if ARG is
positive; otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Org-Pretty-Tags mode is enabled in all buffers where `(lambda nil
(when (derived-mode-p 'org-mode) (org-pretty-tags-mode 1)))' would do
it.

See `org-pretty-tags-mode' for more information on Org-Pretty-Tags
mode.

(fn &optional ARG)" t)
(register-definition-prefixes "org-pretty-tags" '("org-pretty-tags-"))

;;; End of scraped data

(provide 'org-pretty-tags-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; org-pretty-tags-autoloads.el ends here
