;; ***************************************************************************************
(message "Run emacs as server")
;; ***************************************************************************************

(use-package server
  :config
  ;; Start the Emacs server
  (server-start)
  
  ;; Save all open buffers and exit the server process when Emacs is closed
  (add-hook 'kill-emacs-hook 'server-save-buffers-kill-terminal))
;; Alias for emacsclient
(defalias 'e 'server-edit)

;; ***************************************************************************************
(message "Setting package management")
;; ***************************************************************************************
;; Setup package management
(require 'package)
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

(message "================================================================================")
(message "Install use-package if necessary")
(message "================================================================================")
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Set up use-package
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)


(message "================================================================================")
(message "Configure Evil")
(message "================================================================================")
(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode t)
  (setq evil-insert-state-modes (append evil-insert-state-modes '(compilation-mode)))
  (add-hook 'compilation-mode-hook 'evil-normalize-keymaps)
  :bind
  (:map evil-normal-state-map
        ("C-h" . evil-window-left)
        ("C-j" . evil-window-down)
        ("C-k" . evil-window-up)
        ("C-l" . evil-window-right)
        ("C-u" . evil-scroll-up)
        ("C-d" . evil-scroll-down)))


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
    "v" 'evil-window-vsplit
    "l" 'reload-init-file)
  (evil-leader/set-key-for-mode 'emacs-lisp-mode "l" 'reload-init-file))

(message "================================================================================")
(message "reload-init-file")
(message "================================================================================")

(defun reload-init-file ()
  "Unload and reload the user-init-file."
  (interactive)
  (unload-feature 'user-init-file t)
  (load-file (expand-file-name "init.el" user-emacs-directory)))

(message "================================================================================")
(message "Configure Helm")
(message "================================================================================")
(use-package helm
  :ensure t
  :init
  (setq helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t)
  :config
  (helm-mode 1)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x b") 'helm-buffers-list)
  (global-set-key (kbd "<f6>") 'helm-buffers-list)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (setq helm-buffer-max-length 50) ; truncate buffer names to 50 characters
  (setq helm-buffer-details-flag nil) ; hide buffer details by default
  (setq helm-buffer-skip-remote-checking t) ; skip remote buffer checking for faster performance
  (setq helm-buffer-resize-mode-line t) ; resize mode-line to fit buffer names
  (setq helm-autoresize-max-height 30) ; limit helm window height to 30 lines
  (setq helm-autoresize-min-height 10) ; set minimum helm window height to 10 lines
  (helm-autoresize-mode 1) ; auto-resize helm window based on contents
  (setq helm-ff-file-name-history-use-recentf t) ; use recentf file name history
  (setq recentf-max-saved-items 50) ; set maximum number of recent files to keep
  (setq recentf-auto-cleanup 'never) ; disable automatic cleanup of recentf list
  (recentf-mode 1)) ; enable recentf mode

(require 'helm-config)
(helm-mode 1)

(message "================================================================================")
(message "configure helm rg")
(message "================================================================================")

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

(message "================================================================================")
(message "projectile")
(message "================================================================================")
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

(defun my-find-file ()
  (interactive)
  (require 'helm)
  (require 'projectile)
  (if (and (featurep 'projectile) (projectile-project-p))
      (helm-projectile-find-file)
    (helm-find-files-1 default-directory)))
(message "================================================================================")
(message "configure find-file")
(message "================================================================================")


(global-set-key (kbd "<f4>") 'my-find-file)

(global-set-key (kbd "C-<f6>")
                (lambda ()
                  (interactive)
                  (ibuffer t)))

;; (2) Bind F9 to helm-occur
(global-set-key (kbd "<f9>") 'helm-occur)


;; (4) Bind F2 to split window
(global-set-key (kbd "<f2>") 'split-window-below)

;; (5) Bind F10 to switch to another window
(global-set-key (kbd "<f10>") 'other-window)

(global-set-key [f8] 'next-error)

(global-set-key (kbd "C-<f8>") 'previous-error)
(message "================================================================================")
(message "configuring lsp mode")
(message "================================================================================")
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

(message "================================================================================")
(message "Configure Python tools")
(message "================================================================================")
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

(message "================================================================================")
(message "Configure Org")
(message "================================================================================")
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

  ;; :ensure t
  ;; :config
  ;; (setq org-indent-mode 'org-modern-indent)
  ;; (org-modern-mode))
;; (package-disable 'conflicting-package)
(setq org-indent-mode 'org-modern-indent)
(setq org-indent-width 4)

;; ***************************************************************************************
(message "solarized theme setup")
;; ***************************************************************************************
(message "Configure Solarized color theme")
(message "================================================================================")
(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-dark t))

(message "================================================================================")
(message "configurw dap-mode")
(message "================================================================================")
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
(message "================================================================================")
(message "configure popwin")
(message "================================================================================")
(use-package popwin
  :ensure t
  :config
  (popwin-mode 1)
  (global-set-key (kbd "M-n") 'popwin:display-buffer)
  (push '("*scratch*" :dedicated t) popwin:special-display-config))

(message "================================================================================")
(message "configure compilation buffer")
(message "================================================================================")
(setq compilation-environment '("LANG=en_US.UTF-8" "LC_ALL=en_US.UTF-8"))

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (when (eq major-mode 'compilation-mode)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point-max)))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; ***************************************************************************************
(message "vterm setup")
;; ***************************************************************************************
(use-package vterm
  :ensure t
  :commands vterm
  :config
  (setq vterm-shell "/bin/bash")
  ;; Enable mouse mode in vterm
  (add-hook 'vterm-mode-hook (lambda () (vterm-send-string "\e[?1000h")))
  (add-hook 'vterm-exit-hook (lambda () (vterm-send-string "\e[?1000l")))
  (setq vterm-buffer-name-string "vterm %s")
  ;; Set the kill buffer behavior to prevent accidental loss of terminal contents
  (setq vterm-kill-buffer-on-exit t)
  (setq sudo-edit-prompt-function
        (lambda ()
          (concat "Password for " (user-real-login-name) ": ")))
  ;; Use company-mode for tab completion in vterm
  (add-hook 'vterm-mode-hook (lambda () (company-mode)))
  
  ;; Set the keybinding to toggle between line mode and char mode in vterm
  (define-key vterm-mode-map (kbd "C-c C-j") 'vterm-toggle-line-char-mode)
  ;; Enable copy and paste from clipboard in vterm
  (add-hook 'vterm-mode-hook (lambda () (setq-local evil-escape-inhibit t)))
  (add-hook 'vterm-mode-hook (lambda () (setq-local mouse-autoselect-window t)))
  (add-hook 'vterm-mode-hook (lambda () (setq-local select-enable-clipboard t)))
  
  ;; Enable all function keys and key chords in vterm
  (setq vterm-enable-clipboard t))
(message "================================================================================")
(message "configure sudo-edit")
(message "================================================================================")
(use-package sudo-edit
  :ensure t
  :defer t
  :config
  ;; Enable automatic reloading of files after saving
  (setq sudo-edit-auto-reload t)

  ;; Customize the user prompt
  (setq sudo-edit-prompt-function
        (lambda ()
          (concat "Password for " (user-real-login-name) ": ")))

  ;; Set the default command to use when opening files with sudo
  (setq sudo-edit-command "sudo -E emacsclient -t"))

(message "================================================================================")
(message "configure tramp")
(message "================================================================================")
(require 'tramp)
(with-eval-after-load 'tramp
  (use-package tramp
    :config
    (setq tramp-default-method "sshx")
    ;; Increase TRAMP verbosity for debugging
    (setq tramp-verbose 10)
    (setq tramp-debug-buffer t)
    ;; Set TRAMP auto-save and backup directories
    (setq tramp-auto-save-directory "~/tramp-autosave")
    (setq tramp-backup-directory-alist backup-directory-alist)
    ;; Set SSH control master options for TRAMP
    (setq tramp-ssh-controlmaster-options
          "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")
    ;; Ignore TRAMP files for version control
    (setq vc-ignore-dir-regexp (format "%s\\|%s"
                                       vc-ignore-dir-regexp
                                       tramp-file-name-regexp))
    ))



(message "================================================================================")
(message "configure font")
(message "================================================================================")
(set-face-attribute 'default nil
                    :family "Ubuntu Mono"
                    :height 100)
(message "enable clipboard")
;; ***************************************************************************************

(when (and (eq system-type 'gnu/linux)
           (getenv "DISPLAY"))
  (setq interprogram-cut-function 'xclip-cut-function)
  (setq interprogram-paste-function 'xclip-paste-function))

(defun xclip-cut-function (text &optional push)
  (with-temp-buffer
    (insert text)
    (call-process-region
     (point-min) (point-max)
     "xclip" nil 0 nil
     "-i" "-selection" "clipboard")))
    
(defun xclip-paste-function()
  (let ((xclip-output
         (shell-command-to-string "xclip -o -selection clipboard")))
    (unless (string= (car kill-ring) xclip-output)
      xclip-output )))

(message "================================================================================")
(message "configure compile")
(message "================================================================================")

(use-package compile
  :config
  (setq compilation-scroll-output 'first-error)
  (setq compilation-auto-raise-window t)
  (setq compilation-save-buffers-predicate '(lambda () nil))
  (setq compilation-always-kill t)
  (setq compilation-ask-about-save nil)
  (setq compilation-read-command nil)
  (setq compilation-search-path '("." "/usr/local/bin" "/usr/bin" "/bin" "/usr/sbin" "/sbin"))
  (setq compilation-history-file "~/.emacs.d/compile-history")
  (setq compilation-auto-jump-to-first-error t)
  (global-set-key (kbd "<f7>") 'compile)
  (use-package savehist
    :ensure nil
    :config
    (setq savehist-file "~/.emacs.d/savehist")
    (add-to-list 'savehist-additional-variables 'compile-history)))

(message "================================================================================")
(message "configure git")
(message "================================================================================")
(setq-default vc-handled-backends '(Git))
(setq magit-last-seen-setup-instructions "1.4.0")

;; Define a custom function to get the name of the current branch
(defun current-branch ()
  (car (loop for branch in (split-string (shell-command-to-string "git branch") "\n")
             when (string-match "^\*" branch)
             collect (substring branch 2))))

(use-package magit :ensure t)
(require 'magit)
(use-package git-modes :ensure t)
(use-package magit-todos :ensure nil)

(message "================================================================================")
(message "configure eshell")
(message "================================================================================")

(use-package eshell
  :ensure nil
  :hook (eshell-mode . (lambda ()
                         (setq-local eshell-history-size 10000)
                         (setq-local eshell-save-history-on-exit t)
                         (setq-local eshell-ask-to-save-history-before-exit nil)
                         (setq-local eshell-history-file-name "~/.emacs.d/eshell-history")
                         (eshell/alias "ll" "ls -alrt")))
  :config
  (use-package esh-autosuggest
    :hook (eshell-mode . esh-autosuggest-mode)
    :config
    (setq esh-autosuggest-delay 0.5)))

;; (message "================================================================================")
;; (message "configure eshell promp")
;; (message "================================================================================")

;; (setq eshell-prompt-function
;;       (lambda ()
;; 	(require 'magit)
;;         (concat (if (string= (eshell/pwd) (getenv "HOME"))
;;                     "~"
;;                   (abbreviate-file-name (eshell/pwd)))
;;                 (if (magit-get-top-dir (eshell/pwd))
;;                     (concat " [" (magit-get-current-branch) "]")
;;                   "")
;;                 "\n$ ")))

(message "================================================================================")
(message "configure git diff mode")
(message "================================================================================")

;; Integrate meld for visual diffs
(use-package diff-mode
  :ensure nil
  :config
  (setq-default diff-command "git diff --no-ext-diff --no-index --color=always")
  (setq diff-switches "-u")
  (defun my-diff-mode-hook ()
    (when (and (executable-find "meld")
               (string-match-p "^/tmp" (file-truename buffer-file-name)))
      (setq-local diff-command (concat diff-command " --ext-diff --no-prompt"))
      (setq-local diff-switches (concat diff-switches " --diff-program=meld")))))
(add-hook 'diff-mode-hook 'my-diff-mode-hook)
(setq magit-diff-tool '("meld" "--auto-merge" "--diff" "%a" "%b"))
(setq magit-ediff-dwim-show-on-hunks t)
(setq ediff-diff-program "meld")
(setq ediff-diff3-program "meld")

(message "================================================================================")
(message "configure EAF")
(message "================================================================================")
;;(add-to-list 'load-path "~/.local/miniconda3/envs/eaf/lib/python3.9/site-packages/eaf/")
;;(require 'eaf)
;; (require 'emacswiki-elisp-format)
(use-package myeaf
  :ensure t
  :config
  (setq eaf-buffer-file-name "~/.emacs.d/eaf.el")
  (setq eaf-browser-command "firefox")
  (setq eaf-pdf-viewer-command "zathura")
  (setq eaf-git-command "git")
  (setq eaf-file-manager-command "ranger")
  (setq eaf-terminal-command "alacritty")
  (setq eaf-org-previewer-command "emacsclient -n -a '(org-mode)'")
  (setq eaf-system-monitor-command "htop")
  (setq eaf-mermaid-command "mermaid")
  (eaf-initialize))

;; (use-package eaf
;;   :load-path "~/.emacs.d/elpa/eaf-<version>/"
;;   :defer 2
;;   :custom
;;   (eaf-browser-continue-where-left-off t)
;;   (eaf-browser-enable-adblocker t)
;;   (eaf-browser-default-zoom "-")
;;   (eaf-browser-dark-mode "follow")
;;   (eaf-pdf-dark-mode "follow")
;;   (eaf-terminal-font-size "14")
;;   (eaf-find-alternate-file-in-dired t)
;;   (eaf-evil-leader-key "SPC")
;;   :config
;;   (eaf-setq eaf-browser-dark-mode "follow")
;;   (eaf-setq eaf-pdf-dark-mode "follow")
;;   (eaf-setq eaf-browser-default-zoom "2.0")
;;   (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
;;   (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
;;   (eaf-bind-key take_photo "p" eaf-camera-keybinding)
;;   (eaf-bind-key nil "M-q" eaf-browser-keybinding)
;;   (setq eaf-browser-search-engines
;; 	'(("Google" . "https://www.google.com/search?q=%s")
;;           ("Bing" . "https://www.bing.com/search?q=%s")
;;           ("DuckDuckGo" . "https://duckduckgo.com/?q=%s")))
;;   (setq eaf-browser-default-search-engine "Google")
;;   :after eaf
;;   :demand t)
;;   :commands (eaf-open-browser
;;              eaf-open-pdf
;;              eaf-open-git
;;              eaf-open-file-manager
;;              eaf-open-terminal
;;              eaf-preview-org
;;              eaf-open-system-monitor
;;              eaf-open-mermaid)
;;   :custom
;;   (eaf-browser-remember-history t)
;;   (eaf-browser-default-zoom 1.5)
;;   (eaf-browser-dark-mode "follow")
;;   (eaf-pdf-dark-mode "follow")
;;   (eaf-terminal-font-size "12")
;;   :config
;;   (eaf-setq eaf-browser-enable-adblocker t)
;;   (eaf-setq eaf-browser-enable-smooth-scrolling t)
;;   (eaf-setq eaf-pdf-dark-mode "follow")
;;   (eaf-setq eaf-browser-dark-mode "follow")
;;   (eaf-setq eaf-browser-chrome-command "google-chrome-stable")
;;   (eaf-setq eaf-terminal-font-size "12")
;;   :init
;;   (require 'eaf-browser)
;;   (require 'eaf-pdf-viewer)
;;   (require 'eaf-git)
;;   (require 'eaf-file-manager)
;;   (require 'eaf-terminal)
;;   (require 'eaf-org-previewer)
;;   (require 'eaf-system-monitor)
;;   (require 'eaf-mermaid)) 

;; (use-package eaf-pdf-viewer
;;   :after eaf
;;   :config
;;   (setq eaf-pdf-dark-mode "true")
;;   (setq eaf-pdf-dark-mode "follow")
;;   :demand t)

;; (use-package eaf-git
;;   :after eaf
;;   :custom
;;   (eaf-git-evil-leader-key "g")
;;   :config
;;   (setq eaf-git-dired-touch-evil-leader-key "G")
;;   :demand t)

;; (use-package eaf-file-manager
;;   :after eaf
;;   :demand t)

;; (use-package eaf-terminal
;;   :after eaf
;;   :demand t
;;   :config
;;   (setq eaf-terminal-font-size "14"))

;; (use-package eaf-org-previewer
;;   :after eaf
;;   :config
;;   (setq eaf-org-previewer-keybinding-prefix "C-c C-o")
;;   :demand t)

;; (use-package eaf-system-monitor
;;   :after eaf
;;   :demand t)

;; (use-package eaf-mermaid
;;   :after eaf
;;   :demand t)

  
  
  
  
  
  


;; (message "================================================================================")
;; (message "configure git-cola integration")
;; (message "================================================================================")

;; ;; Integrate git-cola for Git GUI
;; (defun my-git-cola ()
;;   (interactive)
;;   ;;(start-process "git-cola" nil "git" "cola"))
;; (global-set-key (kbd "C-c g") 'my-git-cola)
;; ;;(setq magit-git-executable "git-cola")

;; (message "================================================================================")
;; (message "configure treemacs")
;; (message "================================================================================")
;; (use-package treemacs
;;   :ensure t
;;   :defer t
;;   :init
;;   (setq treemacs-no-png-images t) ; use text icons instead of PNGs
;;   :config
;;   (treemacs-git-mode 'deferred) ; enable Git support
;;   (setq treemacs-follow-after-init t
;;         treemacs-is-never-other-window nil
;;         treemacs-silent-refresh t
;;         treemacs-silent-filewatch t
;;         treemacs-show-hidden-files nil
;;         treemacs-goto-tag-strategy 'refetch-index ; update tags on the fly
;;         treemacs-collapse-dirs 0)
;;   (treemacs-resize-icons 18) ; set icon size
;;   (treemacs-follow-mode t) ; enable follow mode
;;   (treemacs-filewatch-mode t) ; enable file watch mode
;;   (treemacs-fringe-indicator-mode t)) ; enable fringe indicators
  ;; (treemacs-define-context-menu-action "Run rg here"
  ;;   :icon "🔍"
  ;;   (lambda (&rest _)
  ;;     (let ((default-directory (treemacs-path-at-point)))
  ;;       (grep-compute-defaults)
  ;;       (rgrep (read-from-minibuffer
  ;;               (format "Run rg on %s: " default-directory)
  ;;               (car grep-find-history))
  ;;              "*.txt"
  ;;              default-directory))))
  ;; ;; Define a custom action to run `fdfind` on a directory
  ;; (treemacs-define-context-menu-action "Run fdfind here"
  ;;   :icon "🔍"
  ;;   (lambda (&rest _)
  ;;     (let ((default-directory (treemacs-path-at-point)))
  ;;       (counsel-fdfind))))
  ;; :bind (:map treemacs-mode-map
  ;;        ;; Bind the custom actions to keys
  ;;        ("C-c r" . treemacs-run-rg)
  ;;        ("C-c f" . treemacs-run-fdfind))))
(treemacs)

;; (use-package treemacs-projectile
;;   :after treemacs projectile
;;   :ensure t)

;; (setq treemacs-show-cursor nil) ; disable the cursor line to show the scroll bar
;; (define-key global-map (kbd "<f10>") (make-sparse-keymap))
;; (define-key global-map (kbd "<f10> t") 'treemacs-toggle)
;; (define-key global-map (kbd "<f10> o") 'other-window)


;; (use-package evil :ensure t)
;; (use-package treemacs-evil :ensure t)

;; (with-eval-after-load 'evil
;;   (evil-set-initial-state 'treemacs-mode 'emacs)
;;   (evil-define-key 'emacs treemacs-mode-map
;;     "h" 'treemacs-root-up
;;     "l" 'treemacs-root-down
;;     "J" 'treemacs-root-collapse
;;     "K" 'treemacs-root-expand
;;     "C-j" 'treemacs-select-next-window
;;     "C-k" 'treemacs-select-previous-window
;;     "C-h" 'evil-window-left
;;     "C-l" 'evil-window-right
;;     "q" 'treemacs-quit))

;; (use-package magit :ensure t)
;; (use-package treemacs :ensure t)
;; (use-package treemacs-magit :ensure t)

;; (with-eval-after-load 'treemacs
;;   (defun my/treemacs-magit-setup ()
;;     "Set up Treemacs-Magit."
;;     (treemacs-load-theme)
;;     (treemacs-git-mode 'extended)
;;     (treemacs-follow-mode t)
;;     (treemacs-filewatch-mode t)
;;     (treemacs-fringe-indicator-mode t)
;;     (treemacs-set-scope-type 'Projects))
;;   (add-hook 'treemacs-mode-hook #'my/treemacs-magit-setup))

;; (setq magit-completing-read-function 'ivy-completing-read
;;       magit-repository-directories '(("~/projects" . 2))
;;       magit-diff-refine-hunk t
;;       magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
;; (require 'treemacs-magit)
;; (setq treemacs-magit--display-in-mode-line t)
;; (setq treemacs-magit--display-in-modeline t)














;; (message "================================================================================")
;; (message "configure treemacs")
;; (message "================================================================================")
;; (use-package treemacs
;;   :ensure t)
;; (require treemacs)
;; (treemacs)

;; (use-package treemacs-projectile
;;   :after treemacs projectile
;;   :ensure t)

;; ;; (setq treemacs-show-cursor nil) ; disable the cursor line to show the scroll bar
;; (define-key global-map (kbd "<f10>") (make-sparse-keymap))
;; (define-key global-map (kbd "<f10> t") 'treemacs-toggle)
;; (define-key global-map (kbd "<f10> o") 'other-window)


;; (use-package evil :ensure t)
;; (use-package treemacs-evil :ensure t)

;; ;; (with-eval-after-load 'evil
;; ;;   (evil-set-initial-state 'treemacs-mode 'emacs)
;; ;;   (evil-define-key 'emacs treemacs-mode-map
;; ;;     "h" 'treemacs-root-up
;; ;;     "l" 'treemacs-root-down
;; ;;     "J" 'treemacs-root-collapse
;; ;;     "K" 'treemacs-root-expand
;; ;;     "C-j" 'treemacs-select-next-window
;; ;;     "C-k" 'treemacs-select-previous-window
;; ;;     "C-h" 'evil-window-left
;; ;;     "C-l" 'evil-window-right
;; ;;     "q" 'treemacs-quit))

;; (use-package magit :ensure t)
;; (use-package treemacs :ensure t)
;; (use-package treemacs-magit :ensure t)

;; ;; (with-eval-after-load 'treemacs
;; ;;   (defun my/treemacs-magit-setup ()
;; ;;     "Set up Treemacs-Magit."
;; ;;     (treemacs-load-theme)
;; ;;     (treemacs-git-mode 'extended)
;; ;;     (treemacs-follow-mode t)
;; ;;     (treemacs-filewatch-mode t)
;; ;;     (treemacs-fringe-indicator-mode t)
;; ;;     (treemacs-set-scope-type 'Projects))
;; ;;   (add-hook 'treemacs-mode-hook #'my/treemacs-magit-setup))

;; ;; (setq magit-completing-read-function 'ivy-completing-read
;; ;;       magit-repository-directories '(("~/projects" . 2))
;; ;;       magit-diff-refine-hunk t
;; ;;       magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
;; (require 'treemacs-magit)
;; ;; (setq treemacs-magit--display-in-mode-line t)
;; ;; (setq treemacs-magit--display-in-modeline t)
(treemacs-load-theme "default")
(message "================================================================================")
(message "completed")
(message "================================================================================")
