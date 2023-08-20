;; -*- mode: emacs-lisp; lexical-binding: t -*-
(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(html
     python
     emacs-lisp
     helm
     multiple-cursors
     treemacs
     git
     lsp
     dap
     org
     eww
     plantuml
     )


   dotspacemacs-additional-packages '(rg helm-rg)
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '()
   dotspacemacs-install-packages 'used-only))
(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  (setq-default
   dotspacemacs-enable-emacs-pdumper nil

   dotspacemacs-emacs-pdumper-executable-file "emacs"

   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

   dotspacemacs-elpa-https t

   dotspacemacs-elpa-timeout 5

   dotspacemacs-gc-cons '(100000000 0.1)

   dotspacemacs-read-process-output-max (* 1024 1024)

   dotspacemacs-use-spacelpa nil

   dotspacemacs-verify-spacelpa-archives t

   dotspacemacs-check-for-update nil

   dotspacemacs-elpa-subdirectory 'emacs-version

   dotspacemacs-editing-style 'vim

   dotspacemacs-startup-buffer-show-version t

   dotspacemacs-startup-banner 'official

   dotspacemacs-startup-banner-scale 'auto

   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   dotspacemacs-startup-buffer-responsive t

   dotspacemacs-show-startup-list-numbers t

   dotspacemacs-startup-buffer-multi-digit-delay 0.4

   dotspacemacs-startup-buffer-show-icons nil

   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   dotspacemacs-scratch-mode 'text-mode

   dotspacemacs-scratch-buffer-persistent nil

   dotspacemacs-scratch-buffer-unkillable nil

   dotspacemacs-initial-scratch-message nil

   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light)

   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   dotspacemacs-colorize-cursor-according-to-state t

   dotspacemacs-default-font '("Source Code Pro"
                               :size 10.0
                               :weight normal
                               :width normal)

   dotspacemacs-leader-key "SPC"

   dotspacemacs-emacs-command-key "SPC"

   dotspacemacs-ex-command-key ":"

   dotspacemacs-emacs-leader-key "M-m"

   dotspacemacs-major-mode-leader-key ","

   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

   dotspacemacs-distinguish-gui-tab nil

   dotspacemacs-default-layout-name "Default"

   dotspacemacs-display-default-layout nil

   dotspacemacs-auto-resume-layouts nil

   dotspacemacs-auto-generate-layout-names nil

   dotspacemacs-large-file-size 1

   dotspacemacs-auto-save-file-location 'cache

   dotspacemacs-max-rollback-slots 5

   dotspacemacs-enable-paste-transient-state nil

   dotspacemacs-which-key-delay 0.4

   dotspacemacs-which-key-position 'bottom

   dotspacemacs-switch-to-buffer-prefers-purpose nil

   dotspacemacs-loading-progress-bar t

   dotspacemacs-fullscreen-at-startup nil

   dotspacemacs-fullscreen-use-non-native nil

   dotspacemacs-maximized-at-startup t

   dotspacemacs-undecorated-at-startup nil

   dotspacemacs-active-transparency 90

   dotspacemacs-inactive-transparency 90

   dotspacemacs-background-transparency 90

   dotspacemacs-show-transient-state-title t

   dotspacemacs-show-transient-state-color-guide t

   dotspacemacs-mode-line-unicode-symbols t

   dotspacemacs-smooth-scrolling t

   dotspacemacs-scroll-bar-while-scrolling t

   dotspacemacs-line-numbers nil
   dotspacemacs-folding-method 'evil

   dotspacemacs-smartparens-strict-mode nil

   dotspacemacs-activate-smartparens-mode t

   dotspacemacs-smart-closing-parenthesis nil

   dotspacemacs-highlight-delimiters 'all

   dotspacemacs-enable-server nil

   dotspacemacs-server-socket-dir nil

   dotspacemacs-persistent-server nil

   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   dotspacemacs-frame-title-format "%I@%S"

   dotspacemacs-icon-title-format nil

   dotspacemacs-show-trailing-whitespace t

   dotspacemacs-whitespace-cleanup nil

   dotspacemacs-use-clean-aindent-mode t

   dotspacemacs-use-SPC-as-y nil

   dotspacemacs-swap-number-row nil

   dotspacemacs-zone-out-when-idle nil

   dotspacemacs-pretty-docs nil

   dotspacemacs-home-shorten-agenda-source nil

   dotspacemacs-byte-compile nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env)
)

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
)


(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
)


(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  ;; (setq plantuml-default-exec-mode 'executable)
  (setq plantuml-default-exec-mode 'x11)
  (setq plantuml-executable-path "/path/to/run-plantuml.sh")
  (setq plantuml-jar-path "/home/ppatoria/plantuml.jar")
  (setq plantuml-xvfb-path "/usr/bin/Xvfb") 
  (setq org-startup-with-inline-images t)
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))
  (message "plantuml-default-exec-mode: %S" plantuml-default-exec-mode)
  (message "plantuml-jar-path: %S" plantuml-jar-path)
  (message "plantuml-xvfb-path: %S" plantuml-xvfb-path)
  ;; (setq org-plantuml-jar-path (expand-file-name "/home/ppatoria/plantuml.jar"))

  (setq menu-bar-mode t)
  (setq tool-bar-mode t)

  (defalias 'll 'eshell/ls "-lart")
  (defalias 'ff 'find-file)

  (global-set-key (kbd "<f2>") 'split-window-below)

  (defun my-find-file ()
    (interactive)
    (require 'helm)
    (require 'projectile)
    (if (and (featurep 'projectile) (projectile-project-p))
        (helm-projectile-find-file)
      (helm-find-files-1 default-directory)))

  (global-set-key (kbd "<f4>") 'my-find-file)
  (global-set-key (kbd "<f6>") 'helm-buffers-list)
  (global-set-key (kbd "<f7>") 'compile)
  (global-set-key [f8] 'next-error)
  (global-set-key (kbd "C-<f8>") 'previous-error)
  (global-set-key (kbd "<f9>") 'helm-occur)
  (global-set-key (kbd "<f10>") 'other-window)
  )

(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(org-preview-html vterm-toggle vterm texfrag auctex plantuml-mode rg helm-rg
                      add-node-modules-path company-web web-completion-data
                      counsel-css emmet-mode helm-css-scss impatient-mode
                      simple-httpd prettier-js pug-mode sass-mode haml-mode
                      scss-mode slim-mode tagedit web-beautify web-mode neotree
                      blacken code-cells company-anaconda anaconda-mode
                      counsel-gtags counsel swiper ivy cython-mode ggtags
                      helm-cscope helm-pydoc importmagic epc ctable concurrent
                      deferred live-py-mode lsp-pyright lsp-python-ms nose
                      pip-requirements pipenv load-env-vars pippel poetry
                      py-isort pydoc pyenv-mode pythonic pylookup pytest pyvenv
                      sphinx-doc stickyfunc-enhance xcscope yapfify eaf
                      ac-ispell auto-complete auto-yasnippet dap-mode lsp-docker
                      bui evil-org flycheck-pos-tip pos-tip fuzzy git-link
                      git-messenger git-modes git-timemachine
                      gitignore-templates gnuplot helm-c-yasnippet helm-company
                      company helm-git-grep helm-ls-git helm-lsp helm-org-rifle
                      htmlize lsp-origami origami lsp-treemacs lsp-ui lsp-mode
                      org-cliplink org-contrib org-download org-mime
                      org-pomodoro alert log4e gntp org-present org-projectile
                      org-category-capture org-rich-yank orgit-forge orgit forge
                      yaml markdown-mode ghub closql emacsql treepy smeargle
                      treemacs-magit magit magit-section git-commit with-editor
                      transient yasnippet-snippets yasnippet ace-jump-helm-line
                      ace-link aggressive-indent all-the-icons auto-compile
                      auto-highlight-symbol centered-cursor-mode
                      clean-aindent-mode column-enforce-mode define-word devdocs
                      dired-quick-sort drag-stuff dumb-jump editorconfig
                      elisp-def elisp-slime-nav emr clang-format list-utils
                      eval-sexp-fu evil-anzu anzu evil-args evil-cleverparens
                      paredit evil-collection annalist evil-easymotion
                      evil-escape evil-exchange evil-goggles evil-iedit-state
                      iedit evil-indent-plus evil-lion evil-lisp-state
                      evil-matchit evil-mc evil-nerd-commenter evil-numbers
                      evil-surround evil-textobj-line evil-tutor evil-unimpaired
                      evil-visual-mark-mode evil-visualstar expand-region
                      eyebrowse fancy-battery flx-ido flx flycheck-elsa
                      flycheck-package package-lint flycheck golden-ratio
                      google-translate helm-ag helm-descbinds helm-make
                      helm-mode-manager helm-org helm-projectile helm-purpose
                      helm-swoop helm-themes helm-xref helm wfnames helm-core
                      help-fns+ hide-comnt highlight-indentation
                      highlight-numbers parent-mode highlight-parentheses
                      hl-todo compat hungry-delete indent-guide info+ inspector
                      link-hint lorem-ipsum macrostep multi-line shut-up
                      nameless open-junk-file org-superstar overseer f pkg-info
                      epl paradox spinner password-generator popup popwin
                      quickrun rainbow-delimiters request restart-emacs
                      smartparens space-doc spaceline powerline
                      spacemacs-purpose-popwin spacemacs-whitespace-cleanup
                      string-edit-at-point string-inflection symbol-overlay
                      symon term-cursor toc-org treemacs-evil
                      treemacs-icons-dired treemacs-persp persp-mode
                      treemacs-projectile treemacs projectile cfrs ht pfuture
                      ace-window avy posframe s undo-tree queue uuidgen
                      vi-tilde-fringe vim-powerline volatile-highlights
                      window-purpose imenu-list winum dash writeroom-mode
                      visual-fill-column ws-butler async bind-map diminish
                      dotenv-mode evil-evilified-state holy-mode hybrid-mode
                      evil goto-chg hydra lv pcre2el which-key)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
