;; Setup package management
(require 'package)
;; ***************************************************************************************
(message "Setting emacs repositories")
;; ***************************************************************************************
;; Increase timeout and number of connection attempts
(setq url-queue-timeout 60)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(setq package--fetch-archives
      '(("gnu" "https://elpa.gnu.org/packages/")
        ("melpa" "https://melpa.org/packages/")
        ("melpa-stable" "https://stable.melpa.org/packages/")
        ("org" "https://orgmode.org/elpa/")
        ("nongnu" "https://elpa.nongnu.org/nongnu/"))
      package-archive-priorities
      '(("melpa-stable" . 10)
        ("gnu" . 5)
        ("melpa" . 0)
        ("org" . 15)
        ("nongnu" . 20)))
(package-initialize)

;; Install use-package if necessary
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Set up use-package
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;; ***************************************************************************************
(message "evil mode setup")
;; ***************************************************************************************
;; Configure Evil
(use-package evil
  :config
  (setq evil-want-keybinding nil)
  (evil-mode 1)
  :bind
  ("C-h" . evil-window-left)
  ("C-j" . evil-window-down)
  ("C-k" . evil-window-up)
  ("C-l" . evil-window-right)
  ("C-u" . evil-scroll-up)
  ("C-d" . evil-scroll-down))

(require 'evil)
(evil-mode 1)
(use-package evil-leader
  :ensure t
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "b" 'switch-to-buffer
    "f" 'find-file
    "k" 'kill-buffer
    "w" 'save-buffer
    "x" 'save-buffers-kill-terminal
    "z" 'undo-tree-visualize
    "g" 'magit-status
    "h" 'split-window-below
    "v" 'split-window-right
    "s" 'evil-window-split
    "v" 'evil-window-vsplit))

;; ***************************************************************************************
(message "helm setup")
;; ***************************************************************************************

;; Configure Helm
(use-package helm
  :config
  (helm-mode 1)
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-x b" . helm-buffers-list)
         ("<f6>" . helm-buffers-list)))

(customize-set-variable 'helm-buffers-fuzzy-matching t)
(require 'helm-config)
(helm-mode 1)

;; (3) Install and configure helm-rg
(use-package helm-rg
  :ensure t
  :config
  (setq helm-rg-default-extra-args '("--hidden"))
  (global-set-key (kbd "<f5>")
                  (lambda ()
                    (interactive)
                    (if (projectile-project-p)
                        (helm-projectile-rg)
                      (helm-do-grep-ag)))))


;; ***************************************************************************************
(message "projectile setup")
;; ***************************************************************************************
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :config
  (setq projectile-completion-system 'helm)
  (use-package helm-projectile
    :ensure t
    :config
    (require 'helm-projectile)
    (helm-projectile-on)))

(eval-after-load 'helm
  '(progn
     (defun my-find-file ()
       (interactive)
       (if (projectile-project-p)
           (helm-projectile-find-file)
         (helm-find-file)))))

;; ***************************************************************************************
(message "global key bindings")
;; ***************************************************************************************
(global-set-key (kbd "<f4>") 'my-find-file)
(global-set-key (kbd "<f6>") 'helm-buffers-list)

(global-set-key (kbd "C-<f6>")
                (lambda ()
                  (interactive)
                  (ibuffer t)))

;; (1) Bind F7 to compile and CTRL + F7 to ripgrep
(global-set-key (kbd "<f7>") 'compile)
(global-set-key (kbd "C-<f7>") 'counsel-rg)

;; (2) Bind F9 to helm-occur
(global-set-key (kbd "<f9>") 'helm-occur)

  
;; (4) Bind F2 to split window
(global-set-key (kbd "<f2>") 'split-window-below)

;; (5) Bind F10 to switch to another window
(global-set-key (kbd "<f10>") 'other-window)

;; ***************************************************************************************
(message "lsp-mode setup")
;; ***************************************************************************************
(use-package lsp-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-python-ms-auto-install-server t
        lsp-python-ms-executable "python3"
	lsp-pylsp-server-command "pylsp"
        lsp-log-io nil)
  :config
  (setq lsp-pylsp-server-command "pylsp")
  (setq lsp-pyls-plugins-flake8-enabled t)
  (setq lsp-enable-snippet nil
        lsp-enable-indentation t
        lsp-enable-symbol-highlighting t
        lsp-enable-on-type-formatting t
        lsp-enable-file-watchers nil
        lsp-prefer-flymake nil
        lsp-pylsp-plugins-auto-ignored '("pylint"))
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.mypy_cache\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.pytest_cache\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.nox\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.tox\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]venv\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]env\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.eggs\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.git\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]__pycache__\\'")
  :bind (:map lsp-mode-map
              ;; ([f2] . lsp-rename)
              ([f12] . lsp-find-definition)
              ([S-f12] . lsp-find-references)
              ([C-f12] . lsp-find-declaration)
              ([M-f12] . lsp-find-implementation)
              ([C-M-f12] . lsp-ui-peek-find-definitions)
              ([C-S-f12] . lsp-ui-peek-find-references)
              ([C-M-S-f12] . lsp-ui-peek-find-implementation)
              ([f1] . lsp-describe-thing-at-point)
              ([C-f1] . lsp-ui-peek-find-documentation)))

(use-package lsp-ui
  :ensure t
  :commands
  lsp-ui-mode)

;; ***************************************************************************************
(message "python setup")
;; ***************************************************************************************
;; Configure Python tools
(use-package python
  :ensure t
  :mode
  ("\\.py\\'" . python-mode)
  :interpreter
  ("python" . python-mode)
  :config
  (setq python-shell-interpreter "python3"))

;; Configure LSP for Python
;; (use-package lsp-pyright
;;   :ensure t
;;   :hook
;;   (python-mode . (lambda ()
;;                    (require 'lsp-pyright)
;;                    (lsp-deferred))))

;; ***************************************************************************************
(message "org-mode setup")
;; ***************************************************************************************
;; Configure Org
(use-package org
  :ensure t
  :mode
  ("\\.org\\'" . org-mode)
  :config
  (setq org-log-done 'time))
(setq org-replace-disputed-keys t)
(add-hook 'org-mode-hook
          (lambda ()
            (setq org-indent-mode t)
            (setq org-indent-boundary-char ?*)
            (setq org-table-auto-convert-tabs nil)
            (setq org-table-default-size "120x")
            (setq org-table-toggle-column-width t)
            ))
(org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     ;; Add more languages as needed
     ))
;; (require 'org)
;; (setq org-startup-indented t) ;; This line is optional, but recommended for better indentation
;; (org-mode)
(use-package org-superstar
  :ensure t
  :config
  (org-superstar-mode))

(use-package org-modern
  :ensure t
  :config
  (setq org-indent-mode 'org-modern-indent)
  (org-modern-mode))
;; (package-disable 'conflicting-package)
(setq org-indent-mode 'org-modern-indent)
(setq org-indent-width 4)

;; ***************************************************************************************
(message "solarized theme setup")
;; ***************************************************************************************
;; Configure Solarized color theme
(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-dark t))

;; ***************************************************************************************
(message "dap setup")
;; ***************************************************************************************
(use-package dap-mode
  :ensure t
  :init
  (require 'dap-python)
  (setq dap-python-executable "python")
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  :bind (:map dap-mode-map
              ;; ("<f9>" . dap-breakpoint-toggle)
              ;; ("<f5>" . dap-debug)
              ;; ("<f10>" . dap-next)
              ;; ("<f11>" . dap-step-in)
              ;; ("<f12>" . dap-step-out)
              ("<M-up>" . dap-up)
              ("<M-down>" . dap-down)
              ;; ("<f6>" . dap-continue)
              ;; ("<f8>" . dap-eval-thing-at-point)
	      )
  :hook ((python-mode . (lambda ()
                          (require 'dap-python)
                          (dap-mode 1)
                          (dap-ui-mode 1)
                          (dap-tooltip-mode 1)
                          (tooltip-mode 1)))))
(package-refresh-contents)
(package-install 'dap-mode)

;; ***************************************************************************************
(message "pcre2el setup")
;; ***************************************************************************************

(use-package pcre2el
  :ensure t
  :config
  (require 'pcre2el))

(menu-bar-mode 1)