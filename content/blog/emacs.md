+++
title = "My Emacs Configuration"
author = ["Shuvrojit Biswas"]
date = 2023-02-20
lastmod = 2023-03-16T17:59:23+06:00
tags = ["emacs", "org"]
categories = ["emacs"]
draft = false
weight = 2001
important = true
+++

This configuration is hacked together from various sources. It satisfies my daily workflow. I'm going to write about my workflow too.


## Startup {#startup}


### Measure startup time {#measure-startup-time}

Garbage collection is a technique used in computer programming to  automatically manage the allocation and release of memory. It involves tracking the objects that are no longer in use by the program and freeing up the memory they occupied. The goal of garbage collection is to reduce the amount of manual memory management required by the programmer and to prevent memory leaks, which can lead to performance problems.

Make startup faster by reducing the frequency of garbage collection and then use a hook to measure Emacs startup time.

```emacs-lisp
;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))


;; Silence compiler warnings as they can be pretty disruptive
(setq comp-async-report-warnings-errors nil)

```


### Emacs Environment  variable {#emacs-environment-variable}

You can set emacs environment variable like this.

```emacs-lisp

(setq debug-on-error t)
(setenv "PATH" (concat (getenv "PATH") "/home/shuvro/.local/bin"))
(setq exec-path (append exec-path '("/home/shuvro/.local/bin")))

(setq user-full-name    "Shuvrojit Biswas"
      user-mail-address "shuvrojit.biswas17@gmail.com")

```


## Package Management {#package-management}


### straight.el {#straight-dot-el}

Package Mangement with `straight.el`.

```emacs-lisp

(defun org-copy-blocks ()
  (interactive)
  (let ((code ""))
    (save-restriction
      (org-narrow-to-subtree)
      (org-babel-map-src-blocks nil
    (setq code (concat code (org-no-properties body)))))
    (kill-new code)))

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
     (url-retrieve-synchronously
      "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
      'silent 'inhibit-cookies)
     (goto-char (point-max))
     (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Always use straight to install on systems other than Linux
(setq straight-use-package-by-default t)

;; Use straight.el for use-package expressions

(straight-use-package 'use-package)
(setq use-package-always-ensure t)

;; Load the helper package for commands like `straight-x-clean-unused-repos'
(require 'straight-x)
```


### Auto update {#auto-update}

```emacs-lisp
(straight-use-package 'org)
(use-package auto-package-update
  :defer 10
  :config
  ;; Delete residual old versions
  (setq auto-package-update-delete-old-versions t)
  ;; Do not bother me when updates have taken place.
  (setq auto-package-update-hide-results t)
  ;; Update installed packages at startup if there is an update pending.
  (auto-package-update-maybe))
```


### Mode Diminishing {#mode-diminishing}

The minions package hides pesky minor modes from the modelines. `diminish` package didn't work?

```emacs-lisp
(use-package minions
  :config
  (minions-mode 1))
```


### Which-key {#which-key}

Which-key will tell us the command name in the minibuffer for all the commands in emacs.

```emacs-lisp
(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.1))
```


## User Interface {#user-interface}


### Hide ugliness {#hide-ugliness}

Disable all the useless startup ui's. It will start in the scratch buffer.

```emacs-lisp
(setq
 inhibit-startup-message t
 visible-bell t
 initial-scratch-message nil
 ;; Save existing clipboard text into the kill ring before replacing it.
 save-interprogram-paste-before-kill t
 ;; when I say to quit, I mean quit
 confirm-kill-processes nil
 )

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
```


### Improve Scrolling {#improve-scrolling}

```emacs-lisp
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
```


### Frame transparency {#frame-transparency}

```emacs-lisp
(set-frame-parameter (selected-frame) 'alpha '(99 . 99))
(add-to-list 'default-frame-alist '(alpha . (99 . 99)))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
```


### Line numbers {#line-numbers}

```emacs-lisp
(column-number-mode)

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode t))))

;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
```

```emacs-lisp
;Don't warn for large files (shows up when launching videos)
(setq large-file-warning-threshold nil)

;;Don't warn for following symlinked files
(setq vc-follow-symlinks t)

;Don't warn when advice is added for functions
;;(setq ad-redefinition-action 'accept)
```


### Modeline {#modeline}

I'm currently using `telephone-line` as modeline. In the past, I have used `doom-modeline`.

```emacs-lisp
(use-package telephone-line)
(setq telephone-line-primary-left-separator 'telephone-line-cubed-left
      telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
      telephone-line-primary-right-separator 'telephone-line-cubed-right
      telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
(setq telephone-line-height 24
      telephone-line-evil-use-short-tag t)
(telephone-line-mode 1)
```


### Font {#font}

need to write something about fonts.

```emacs-lisp
;; Set the fixed pitch face
;; sets up org source blocks
(set-face-attribute 'fixed-pitch nil
                    :font "Hack"
                    :weight 'light
                    :height 150
                    )
;;sets up modeline text, code text
(set-face-attribute 'default nil
                    :font "Hack"
                    ;; :font "Cascadia Code"
                    :height 150
                    )

;; Set the variable pitch face
;;sets up org mode header
(set-face-attribute 'variable-pitch nil
                    ;; :font "Cantarell"
                    ;; :font "Inconsolata"
                    ;; :font "ETbookOT"
                    :font "Fira Code"
                    ;; :font "Times New Roman"
                    :weight 'light
                    :height 170)
```


### ef-themes {#ef-themes}

```emacs-lisp
;; (require 'ef-themes)
;; The themes we provide:
;;
;; Light: `ef-day', `ef-light', `ef-spring', `ef-summer'.
;; Dark:  `ef-autumn', `ef-dark', `ef-night', `ef-winter'.

;; We also provide these commands, but do not assign them to any key:
;;
;; - `ef-themes-select'
;; - `ef-themes-load-random'
;; - `ef-themes-preview-colors'
;; - `ef-themes-preview-colors-current'

(when (display-graphic-p)

(use-package ef-themes
  :config
  ;; Disable all other themes to avoid awkward blending:
  (mapc #'disable-theme custom-enabled-themes)

  ;; Load the theme of choice:
  (load-theme 'ef-spring' :no-confirm)
  (load-theme 'ef-winter' :no-confirm)
  (load-theme 'ef-summer' :no-confirm)
  (load-theme 'ef-night' :no-confirm)
  (load-theme 'ef-day' :no-confirm)
  (load-theme 'ef-autumn' :no-confirm)
  )

(ef-themes-load-random 'dark)
)
(if (not (display-graphic-p)) (load-theme 'default t))
```


### Beacon {#beacon}

A beacon shinning on your point man.

```emacs-lisp
(use-package beacon
  :init
  (setq beacon-blink-when-point-moves t)
  (setq beacon-blink-when-window-change t)
  (setq beacon-blink-when-window-scrolls t)
  (beacon-mode 1))
```


### Emoji {#emoji}

Emoji support on emacs. :)

```emacs-lisp
(use-package emojify
  :hook (erc-mode . emojify-mode))
(add-hook 'after-init-hook #'global-emojify-mode)

```


### All the icons {#all-the-icons}

```emacs-lisp
(use-package all-the-icons)
```


### Dashboard {#dashboard}

A dashboard that's gonna open everytime you open emacs

```emacs-lisp
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))
```


### Emacs Mini frame {#emacs-mini-frame}

```emacs-lisp
(use-package mini-frame)
(setq x-gtk-resize-child-frames 'resize-mode)
(custom-set-variables
 '(mini-frame-show-parameters
   '((top . 400)
     (width . 0.7)
     (left . 0.5))))
(mini-frame-mode 1)

```


## Base Config {#base-config}


### Evil mode {#evil-mode}

Set `evil-want-keybinding` variable to `nil` before initializing evil mode or it will produce and error. Evil collection is not going to work without doing that.

```emacs-lisp
 (defun em/evil-hook ()
  (dolist (mode '(custom-mode
                  eshell-mode
                  vterm-mode
                  git-rebase-mode
                  ;; erc-mode
                  ;; circe-server-mode
                  ;; circe-chat-mode
                  ;; circe-query-mode
                  ;; sauron-mode
                  term-mode))
  (add-to-list 'evil-emacs-state-modes mode)))


(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-tree)
  :config
  (add-hook 'evil-mode-hook 'em/evil-hook)
  (evil-mode 1)
  (setcdr evil-insert-state-map nil)
  (define-key evil-insert-state-map [escape] 'evil-normal-state)
  (define-key evil-normal-state-map (kbd "C-t") 'shell-pop)
(define-key evil-normal-state-map (kbd "M-.") 'xref-find-definitions)


  (define-key evil-normal-state-map (kbd "C-.") 'nil)
  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)

  (evil-global-set-key 'motion "U" 'evil-redo)

  (evil-global-set-key 'motion "t" (dired "./"))


  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)


  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :init
  (setq evil-collection-company-use-tng nil)  ;; Is this a bug in evil-collection?
  :custom
  (evil-collection-outline-bind-tab-p nil)
  :config
  (setq evil-collection-mode-list
        (remove 'lispy evil-collection-mode-list))
  (evil-collection-init))


(define-key evil-normal-state-map (kbd "C-e") 'end-of-line)

(use-package general
  :config
  (general-evil-setup t)

  (general-create-definer evil/leader-keys
                          :keymaps '(normal insert visual emacs)
                          :prefix "SPC"
                          :global-prefix "C-SPC")

  (evil/leader-keys
   "t" '(:ignore t :which-key "toggles")
   ;; "tt"'(counsel-load-theme :which-key "choose theme")
   "a" '(org-agenda :which-key "Org Agenda")
   "w" '(evil-window-map :which-key "Evil window map")
   ;;files
   "f" '(:ignore f :which-key "files")
   "ff"'(find-file :which-key "find file")
   "fs"'(save-buffer :which-key "save buffer")
   "h" '(help-command :which-key "help command")
                                        ;buffers
   "b" '(:ignore b :which-key "buffers")
   "br"'(revert-buffer :which-key "revert buffer")
   "bk"'(previous-buffer :which-key "previous buffer")
   "bj"'(next-buffer :which-key "next buffer")
   "be"'(eval-buffer :which-key "Eval Buffer")
   "s" '(bookmark-map s :which-key "bookmark")

   ))

(evil/leader-keys
 "n" '(:which-key "org roam")
 "na"'(org-roam-buffer-toggle :which-key "org roam toggle")
 "nf"'(org-roam-node-find :which-key "org roam find")
 "ni"'(org-roam-node-insert :which-key "org roam node insert")
 )


```


#### Surround Vim {#surround-vim}

```emacs-lisp

(use-package evil-surround
  :after evil
  :defer 2
  :config
  (global-evil-surround-mode 1))

```


### Keep Backup files clean {#keep-backup-files-clean}

```emacs-lisp

;; backup files directory for per project
;;(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

;; Auto save files
;; New location for backups.
(setq backup-directory-alist '(("." . "~/.config/emacs/backups")))

;; Silently delete execess backup versions
(setq delete-old-versions t)

;; Only keep the last 1000 backups of a file.
(setq kept-old-versions 100)

;; Even version controlled files get to be backed up.
(setq vc-make-backup-files t)

;; Use version numbers for backup files.
(setq version-control t)

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))

;; Use no-littering to automatically set common paths to the new user-emacs-directory
(use-package no-littering)

;; Keep customization settings in a temporary file (thanks Ambrevar!)
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

```


### an elementary diff system for backups. {#an-elementary-diff-system-for-backups-dot}

```emacs-lisp
(use-package backup-walker
  :commands backup-walker-start)
```


### Make backups everytime {#make-backups-everytime}

Emacs only makes a backup the very first time a buffer is saved; I’d prefer Emacs makes backups everytime I save! —If I saved, that means I’m at an important checkpoint, so please check what I have so far as a backup!

```emacs-lisp
(defun force-backup-of-buffer ()
  (setq buffer-backed-up nil))

(add-hook 'before-save-hook  'force-backup-of-buffer)

;;alternative
;;(defun backup-and-save ()
 ;; (interactive)
 ;; (setq filename (buffer-file-name))
 ;; (write-file (concat filename (format-time-string "_" "%Y%m%d%H%M%S")))
 ;; (write-file filename)
 ;; )

```


### Auto-Reverting Changed Files {#auto-reverting-changed-files}

```emacs-lisp
;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)
```


### Matching Braces {#matching-braces}

```emacs-lisp
(use-package paren
  :config
  (set-face-attribute 'show-paren-match-expression nil :background "#363e4a")
  (show-paren-mode 1))
```


### Tab width {#tab-width}

```emacs-lisp
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
;; Make tab key do indent first then completion.
(setq-default tab-always-indent 'complete)
```


### Comments {#comments}

`comment-dwim-2` is a package that extends the functionality of built-in `comment-dwim`.

```emacs-lisp
(use-package comment-dwim-2
  :bind (("M-;" . comment-dwim-2)
         (:map org-mode-map
               ("M-;" . org-comment-dwim-2))))
```

`evil-nerd-commenter` is a better option if you're from vim style keybindings and workflow. It has extra functionality like a bunch of extra function. Read it in the github page.

```emacs-lisp
(use-package evil-nerd-commenter
  :bind (("M-;" . evilnc-comment-or-uncomment-lines)))
```


### Auto clean whitespace {#auto-clean-whitespace}

```emacs-lisp
(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))
```


### Auto-Saving Changed Files {#auto-saving-changed-files}

```emacs-lisp
(use-package super-save
  :defer 1
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle nil))

;; (use-package multiple-cursors)

```


### Smart Parens {#smart-parens}

```emacs-lisp
(use-package smartparens
  :hook (prog-mode . smartparens-mode))

```


### Rainbow Mode {#rainbow-mode}

Sets the background of HTML color strings in buffers to be the color mentioned.

```emacs-lisp
(use-package rainbow-mode
  :defer t
  :hook (org-mode
         emacs-lisp-mode
         web-mode
         python-mode
         prog-mode))

```


### Rainbow Delimiters {#rainbow-delimiters}

```emacs-lisp
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

```


### openwith {#openwith}

```emacs-lisp
(use-package openwith
  :config
  (setq openwith-associations
        (list
          (list (openwith-make-extension-regexp
                '("mpg" "mpeg" "mp3" "mp4"
                  "avi" "wmv" "wav" "mov" "flv"
                  "ogm" "ogg" "mkv"))
                "mpv"
                '(file))
          (list (openwith-make-extension-regexp
                '("xbm" "pbm" "pgm" "ppm" "pnm"
                  "png" "gif" "bmp" "tif" "jpeg")) ;; Removed jpg because Telega was
                  ;; causing feh to be opened...
                  "sxiv"
                  '(file))
          (list (openwith-make-extension-regexp
                '("pdf"))
                "zathura"
                '(file)))))
```


### Restart Emacs {#restart-emacs}

```emacs-lisp
;; Provides only the command “restart-emacs”.
(use-package restart-emacs
  ;; If I ever close Emacs, it's likely because I want to restart it.
  :bind ("C-x C-c" . restart-emacs)
  ;; Let's define an alias so there's no need to remember the order.
  :config (defalias 'emacs-restart #'restart-emacs))

;; Keep open files open across sessions.
;;(desktop-save-mode 1)
;;(setq desktop-restore-eager 10)

;; saves the last place the point was on a buffer during visit.
(save-place-mode 1)
(setq save-place-forget-unreadable-files nil)
```


### Undo {#undo}

Undo-tree

```emacs-lisp
(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode 1)
  :config
  (defalias 'redo 'undo-tree-redo)
  ;;setting the default backed up file list for undo
  (setq undo-tree-history-directory-alist '(("." . "~/.config/emacs/undo")))
  :bind (("C-x u" . undo)     ; Zap to character isn't helpful
         ("C-x U" . redo)))

```

**Undo-fu**

```emacs-lisp
(use-package undo-fu
  :config
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z")   'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))
```


### Magit {#magit}

```emacs-lisp
(use-package magit
  :bind ("C-x g" . magit-status)
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(when (equal ""
(shell-command-to-string "git config user.name"))
  (shell-command "git config --global user.name \"Shuvrojit Biswas\"")
  (shell-command "git config --global user.email \"jsuvro17@gmail.com\""))

(use-package git-timemachine :defer t)
(evil/leader-keys
  "g"   '(:ignore t :which-key "git")
  "gs"  'magit-status
  "gd"  'magit-diff-unstaged
  "gc"  'magit-branch-or-checkout
  "gl"   '(:ignore t :which-key "log")
  "glc" 'magit-log-current
  "glf" 'magit-log-buffer-file
  "gb"  'magit-branch
  "gP"  'magit-push-current
  "gp"  'magit-pull-branch
  "gf"  'magit-fetch
  "gF"  'magit-fetch-all
  "gr"  'magit-rebase)


```


### Dired {#dired}

```emacs-lisp
(use-package all-the-icons-dired)

(use-package dired
  :ensure nil
  :straight nil
  :defer 1
  :commands (dired dired-jump)
  :config
  ;; (setq dired-listing-switches "-agho --group-directories-first"
  ;;       dired-omit-files "^\\.[^.].*"
  ;;       dired-omit-verbose nil
  ;;       dired-hide-details-hide-symlink-targets nil
  ;;       delete-by-moving-to-trash t)

  (autoload 'dired-omit-mode "dired-x")

  (add-hook 'dired-load-hook
            (lambda ()
              (interactive)
              (dired-collapse)))

  (add-hook 'dired-mode-hook
            (lambda ()
              (interactive)
              (dired-omit-mode 1)
              (dired-hide-details-mode 1)
              (all-the-icons-dired-mode 1))
              (hl-line-mode 1))

  (use-package dired-rainbow
    :defer 2
    :config
    (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
    (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
    (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
    (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
    (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
    (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
    (dired-rainbow-define media "#de751f" ("mp3" "mp4" "mkv" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
    (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
    (dired-rainbow-define log "#c17d11" ("log"))
    (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
    (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
    (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
    (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
    (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
    (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
    (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
    (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
    (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
    (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
    (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*"))

  (use-package dired-single
    :defer t)

  (use-package dired-ranger
    :defer t)

  (use-package dired-collapse
    :defer t)

  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "H" 'dired-omit-mode
    "l" 'dired-single-buffer
    "y" 'dired-ranger-copy
    "d" 'dired-ranger-move
    "p" 'dired-ranger-paste))

  ;; (defun em/dired-link (path)
  ;;   (lexical-binding ((target path))
  ;;    (lambda () (interactive) (message "Path: %s" target) (dired target))))

  ;; (evil/leader-keys
  ;;  "d"   '(:ignore t :which-key "dired")
  ;;   "dd"  '(dired :which-key "Here")
  ;;   "dh"  `(,(em/dired-link "~") :which-key "Home")
  ;;   "dn"  `(,(em/dired-link "~/Desktop") :which-key "Desktop")
  ;;   "db"   ((em/dired-link "~/Desktop/books/"):which-key "Books")
  ;;   "do"  `(,(em/dired-link "~/Downloads") :which-key "Downloads")
  ;;   "d."  `(,(em/dired-link "~/repos/.dotfiles") :which-key "dotfiles"))
  ;;   "de"  `(,(em/dired-link "~/.config/emacs") :which-key ".emacs.d"))

(setq dired-recursive-copies 'top)

(setq dired-recursive-deletes 'top)
(setq dired-dwim-target t)


```


## Workspace {#workspace}


### Buffer {#buffer}

Projectile will take care of the project buffers. But I need something to work on non project things. Like when I am not in project mode and and just opening a file. I need the old buffer system of emacs.

```emacs-lisp
(global-unset-key (kbd "C-x l"))
```


### Perspective {#perspective}

```emacs-lisp
(use-package perspective
  :init
  (setq-default persp-suppress-no-prefix-key-warning t)
  (persp-mode)
  :config
  (persp-turn-on-modestring))

(defun persp1()
  (interactive)
  (persp-switch-by-number 1))

(defun persp2()
  (interactive)
  (persp-switch-by-number 2))

(defun persp3()
  (interactive)
  (persp-switch-by-number 3))

(defun persp4()
  (interactive)
  (persp-switch-by-number 4))

(global-set-key (kbd "M-1") 'persp1)
(global-set-key (kbd "M-2") 'persp2)
(global-set-key (kbd "M-3") 'persp3)
(global-set-key (kbd "M-4") 'persp4)

```


### Projectile {#projectile}

```emacs-lisp
(defun em/switch-project-action ()
  "Switch to a workspace with the project name and start `magit-status'."
  ;; TODO: Switch to EXWM workspace 1?
  (persp-switch (projectile-project-name))
  (magit-status))

(use-package projectile
  :config (projectile-mode)
  :demand t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
(setq projectile-track-down-projects-automatically nil)
  (when (file-directory-p "~/Desktop/projects/")
    (setq projectile-project-search-path '("~/Desktop/projects/")))
  (setq projectile-switch-project-action #'em/switch-project-action))


```


### Window {#window}

Ace Window

```emacs-lisp
(use-package ace-window
  :config
(setq aw-ignore-current t)
  )
(global-set-key (kbd "C-x o") 'ace-window)
```

Window history with winner.

```emacs-lisp
(use-package winner
  :after evil
  :config
  (winner-mode)
  (define-key evil-window-map "u" 'winner-undo)
  (define-key evil-window-map "U" 'winner-redo))

```


### Calendar {#calendar}

calfw is a gorgeous calendar UI that is able to show all of my scheduled Org Agenda items.

```emacs-lisp
(use-package calfw
  :commands cfw:open-org-calendar
  :config
  (setq cfw:fchar-junction ?╋
        cfw:fchar-vertical-line ?┃
        cfw:fchar-horizontal-line ?━
        cfw:fchar-left-junction ?┣
        cfw:fchar-right-junction ?┫
        cfw:fchar-top-junction ?┯
        cfw:fchar-top-left-corner ?┏
        cfw:fchar-top-right-corner ?┓)

  (use-package calfw-org
    :config
    (setq cfw:org-agenda-schedule-args '(:timestamp))))

(evil/leader-keys
  "cc"  '(cfw:open-org-calendar :which-key "calendar"))
```


### Indent style {#indent-style}

```emacs-lisp

(use-package highlight-indent-guides
  :init (setq highlight-indent-guides-method 'character)
        (setq highlight-indent-guides-responsive 'stack)
  :hook (prog-mode . highlight-indent-guides-mode))


```


### Bookmark {#bookmark}

```emacs-lisp
(use-package bookmark+)

(global-set-key (kbd "C-w") 'evil-window-map)
(define-key evil-window-map (kbd "r") 'bmkp-desktop-jump)
(define-key evil-window-map (kbd "d") 'bmkp-desktop-delete)

;; (global-set-key (kbd "s") 'bkmp-set-desktop-bookmark )
```

```emacs-lisp
;; (use-package bm
  ;; :bind (("M-h" . bm-toggle)
         ;; ("M-j" . bm-lifo-next)
         ;; ("M-k" . bm-lifo-previous)
         ;; ("M-l" . bm-show-all)
         ;; ))


;; (use-package goto-chg
;;   :bind (
;;          ("M-k" . goto-last-change)
;;          ("M-j" . goto-last-change-reverse)
;;          ))


(use-package bm
         :ensure t
         :demand t

         :init
         ;; restore on load (even before you require bm)
         (setq bm-restore-repository-on-load t)


         :config
         ;; Allow cross-buffer 'next'
         (setq bm-cycle-all-buffers t)

         ;; where to store persistant files
         (setq bm-repository-file "~/.emacs.d/bm-repository")

         ;; save bookmarks
         (setq-default bm-buffer-persistence t)

         ;; Loading the repository from file when on start up.
         (add-hook 'after-init-hook 'bm-repository-load)

         ;; Saving bookmarks
         (add-hook 'kill-buffer-hook #'bm-buffer-save)

         ;; Saving the repository to file when on exit.
         ;; kill-buffer-hook is not called when Emacs is killed, so we
         ;; must save all bookmarks first.
         (add-hook 'kill-emacs-hook #'(lambda nil
                                          (bm-buffer-save-all)
                                          (bm-repository-save)))

         ;; The `after-save-hook' is not necessary to use to achieve persistence,
         ;; but it makes the bookmark data in repository more in sync with the file
         ;; state.
         (add-hook 'after-save-hook #'bm-buffer-save)

         ;; Restoring bookmarks
         (add-hook 'find-file-hooks   #'bm-buffer-restore)
         (add-hook 'after-revert-hook #'bm-buffer-restore)

         ;; The `after-revert-hook' is not necessary to use to achieve persistence,
         ;; but it makes the bookmark data in repository more in sync with the file
         ;; state. This hook might cause trouble when using packages
         ;; that automatically reverts the buffer (like vc after a check-in).
         ;; This can easily be avoided if the package provides a hook that is
         ;; called before the buffer is reverted (like `vc-before-checkin-hook').
         ;; Then new bookmarks can be saved before the buffer is reverted.
         ;; Make sure bookmarks is saved before check-in (and revert-buffer)
         (add-hook 'vc-before-checkin-hook #'bm-buffer-save)


         :bind (("M-j" . bm-next)
                ;; ("M-k" . bm-previous)
                ("M-h" . bm-toggle))
         ("M-l" . bm-show-all)
         )


```


### Folding {#folding}

```emacs-lisp
(use-package origami
  :config
  (global-origami-mode 1))

(use-package vimish-fold
  :bind (("C-x w" . vimish-fold)
         ("C-x y" . vimish-fold-delete-all)))
```


### Avy {#avy}

```emacs-lisp
(use-package goto-last-change)
(use-package avy
  :config
  (global-set-key (kbd "M-o") 'avy-goto-word-or-subword-1)
)

```

nice and easy huh? That's the world we live in and so on...


### Register {#register}

**C-c C-c** uses `mode-specific-command-prefix`. It can be used to bind other keymaps prefix. Because it doesn't do anything by itself.

```emacs-lisp
  (global-unset-key (kbd "C-q"))
(global-unset-key (kbd "C-r"))
(define-key evil-normal-state-map (kbd "C-r") 'nil)
(global-set-key (kbd "C-r") 'mode-specific-command-prefix)
  (global-set-key (kbd "C-r r") 'point-to-register)
  (global-set-key (kbd "C-r w") 'window-configuration-to-register)
  (global-set-key (kbd "C-r e") 'copy-to-register)



```


## Shell {#shell}

```emacs-lisp
(use-package vterm
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000))

;; (define-key vterm-mode-map (kbd "M-1") 'persp1)
;; (define-key vterm-mode-map (kbd "M-2") 'persp2)
;; (define-key vterm-mode-map (kbd "M-3") 'persp3)
;; (define-key vterm-mode-map (kbd "M-4") 'persp4)
```

```emacs-lisp
(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))
```

```emacs-lisp
(setq shell-file-name "/bin/zsh")
  (use-package shell-pop
    :defer t
    :custom
    ;; Percentage for shell-buffer window size.
    (shell-pop-window-size 30)
    ;; Position of the popped buffer: top, bottom, left, right, full.
    (shell-pop-window-position "bottom")
    (shell-pop-term-shell "/bin/zsh"))
```

```emacs-lisp

(use-package shell-pop
  :bind (("C-t" . shell-pop))
  :config
  (setq shell-pop-shell-type (quote ("vterm" "vterm" (lambda nil (vterm shell-pop-term-shell)))))
  ;;(setq shell-pop-set-internal-mode "term")
  (setq shell-pop-term-shell "/bin/zsh")
  ;; need to do this manually or not picked up by `shell-pop'
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))

```


## Tools {#tools}


### Docker {#docker}

```emacs-lisp

;; for managing docker within emacs
(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

;; for dockerfile
(use-package dockerfile-mode)
(use-package docker-compose-mode)

```

```emacs-lisp
(use-package ob-docker-build
  :ensure t
  :defer t
  :straight (ob-docker-build :type git
                             :host github
                             :repo "ifitzpat/ob-docker-build")
  :config
  (add-to-list 'org-babel-load-languages '(docker-build . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
)
```


### Live Reload {#live-reload}

For live reloading of the files `Impatient-mode` is amazing. You just need to install two packages. After that you enable impatient mode on html files and additional css files and go to [http://localhost:8000/imp](http://localhost:8080/imp/) and select the buffer you want to edit. And it's that easy. Just start `httpd-start` before doing any of this.

```emacs-lisp
(use-package simple-httpd)
;; (use-package js2-mode)
;; (use-package skewer-mode)
;; (add-hook 'js2-mode-hook 'skewer-mode)
;; (add-hook 'css-mode-hook 'skewer-css-mode)
;; (add-hook 'html-mode-hook 'skewer-html-mode)


(use-package impatient-mode)

```


### Elfeed {#elfeed}

```emacs-lisp
(use-package elfeed
  :commands elfeed
  :config
  (setq elfeed-feeds
    '("https://nullprogram.com/feed/"
      "https://ambrevar.xyz/atom.xml"
      "https://guix.gnu.org/feeds/blog.atom"
      "https://valdyas.org/fading/feed/"
      "https://www.reddit.com/r/emacs/.rss")))
```


### Pdf viewer {#pdf-viewer}

```emacs-lisp
(use-package pdf-tools
  :defer t
  ; :init   (system-packages-ensure "pdf-tools")
  :custom (pdf-tools-handle-upgrades nil)
          (pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo")
  :config (pdf-tools-install))

```


### Sauron {#sauron}

Sauron mode for managing emacs notifications.

```emacs-lisp
(use-package sauron)
```


### IRC {#irc}

Internet Relay Chat.

```emacs-lisp
(defvar weechat-formatting-regex
  (rx-let ((attr (in "*!/_|"))   ;NOTE:  is not documented
         (std  (= 2 digit))
         (astd (seq attr (= 2 digit)))
         (ext  (seq "@" (= 5 digit)))
         (aext (seq "@" attr (= 5 digit))))
    (rx
     (or (seq ""
               (or std
                   ext
                   (seq "F" (or std astd ext aext))
                   (seq "B" (or std ext))
                   (seq "*" (or std
                                astd
                                ext
                                aext
                                (seq (or std astd ext aext)
                                     ","
                                     (or std astd ext aext))))
                   (seq "b" (in "-FDB#_il"))
                   ""))
          (seq "" attr)
          (seq "" attr)
          ""))))

(use-package weechat)

```

```emacs-lisp
(use-package circe)

(setq circe-network-options
      '(("Libera"
         :tls t
         :nick "testing"
         :sasl-username "testing"
         :sasl-password "my-password"
         :channels ("#emacs-circe","#emacs")
         )))



```


### Keycast {#keycast}

To show which keys you're pressing right now. Taken from emacswiki.

```emacs-lisp
    ;; (keycast-tab-bar-mode 1)
    ;; (keycast-log-mode 1)

  (use-package keycast
    :init
    (add-to-list 'global-mode-string '("" mode-line-keycast))
    )
```


## OrgMode {#orgmode}


### UI {#ui}

```emacs-lisp
(setq org-hide-emphasis-markers t)
(add-hook 'org-mode-hook (lambda ()
                           "Beautify Org Checkbox Symbol"
                           (push '("[ ]" .  "☐") prettify-symbols-alist)
                           (push '("[X]" . "☑" ) prettify-symbols-alist)
                           (push '("[-]" . "❍" ) prettify-symbols-alist)
                           (push '("#+BEGIN_SRC" . "↦" ) prettify-symbols-alist)
                           (push '("#+END_SRC" . "⇤" ) prettify-symbols-alist)
                           (push '("#+BEGIN_EXAMPLE" . "↦" ) prettify-symbols-alist)
                           (push '("#+END_EXAMPLE" . "⇤" ) prettify-symbols-alist)
                           (push '("#+BEGIN_QUOTE" . "↦" ) prettify-symbols-alist)
                           (push '("#+END_QUOTE" . "⇤" ) prettify-symbols-alist)
                           (push '("#+begin_quote" . "↦" ) prettify-symbols-alist)
                           (push '("#+end_quote" . "⇤" ) prettify-symbols-alist)
                           (push '("#+begin_example" . "↦" ) prettify-symbols-alist)
                           (push '("#+end_example" . "⇤" ) prettify-symbols-alist)
                           (push '("#+begin_src" . "↦" ) prettify-symbols-alist)
                           (push '("#+end_src" . "⇤" ) prettify-symbols-alist)
                           (prettify-symbols-mode)))



(font-lock-add-keywords 'org-mode
                        '(("^ +\\([-*]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(add-hook 'org-mode-hook 'variable-pitch-mode)
(add-hook 'org-mode-hook 'visual-line-mode)


(setq org-startup-indented t
      org-src-tab-acts-natively t)



```


### Font {#font}

```emacs-lisp

(when (display-graphic-p)
(let* ((variable-tuple (cond ((x-list-fonts "ETBookOT")   '(:font "ETBookOT"))
			                 ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                             ((x-list-fonts "Verdana")         '(:font "Verdana"))
                             ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                             (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

  (custom-theme-set-faces 'user
                          `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
                          `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.15))))
                          `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.20))))
                          `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.25))))
                          `(org-document-title ((t (,@headline ,@variable-tuple :height 1.3 :underline nil))))))

(use-package org-bullets
  :custom
  (org-bullets-bullet-list '( "●" "◯" "☉" "⊛"   ))
  (org-ellipsis " ▼")
  :hook (org-mode . org-bullets-mode))

(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "ETBembo" :height 180 :weight normal))))
 '(fixed-pitch ((t ( :family "Fira Code Retina" :height 150)))))

(custom-theme-set-faces
 'user
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

(setq org-emphasis-alist
      (quote (("*" bold)
              ("/" italic)
              ("_" underline)
              ("=" (:foreground "#616161" :background "#fff59d"))
              ("~" org-verbatim verbatim)
              ("+"
               (:strike-through t))
              )))
)
```


### Org Roam {#org-roam}

```emacs-lisp
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Desktop/repos/braindump")
  (org-roam-dailies-directory "journal/")
  (org-roam-completion-everywhere nil)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n r" . org-roam-node-random)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow)
         )
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies)
  (org-roam-db-autosync-mode)
  ;; (setq org-roam-database-connector 'sqlite3)
  (org-roam-setup))

```


### Org Roam UI {#org-roam-ui}

```emacs-lisp
(use-package org-roam-ui
  :straight
  (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

```


### Org tempo {#org-tempo}

```emacs-lisp
(require 'org-tempo)

;; shortcut for org-source blog template
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("p" . "src python"))
(add-to-list 'org-structure-template-alist '("r" . "src rust"))
(add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
(add-to-list 'org-structure-template-alist '("g" . "src go"))
(add-to-list 'org-structure-template-alist '("hb" . "src html-chrome"))
(add-to-list 'org-structure-template-alist '("cs" . "src css"))





```


### Ob-browser {#ob-browser}

```emacs-lisp

(use-package ob-html-chrome
  :ensure t
  :init
  (setq org-babel-html-chrome-chrome-executable "/usr/bin/google-chrome-stable")
  :config (setq org-confirm-babel-evaluate
      (lambda (lang body)
        (not (string= lang "html-chrome")))))

(setq org-confirm-babel-evaluate nil)
```


### Org babel {#org-babel}

```emacs-lisp
  (use-package ob-rust)
  (use-package ob-typescript)
(use-package ob-go)
  ;; load org bable languages emacs lisp and python
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t)
     (rust . t)
     (typescript . t)
     (go . t)
    (html-chrome . t)
     ))

  ;; org-babel confirmation msg hide
  ;; org-babel src block indentation level
  (setq org-src-preserve-indentation t)

```


### Set Margins for Modes {#set-margins-for-modes}

```emacs-lisp

;; (defun dw/center-buffer-with-margins ()
;;   (let ((margin-size (/ (- (frame-width) 80) 3)))
;;     (set-window-margins nil margin-size margin-size)))

(defun em/org-mode-visual-fill ()
  (setq visual-fill-column-width 60
        visual-fill-column-center-text t)
  (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust)

  (adaptive-wrap-prefix-mode 1)
  (visual-fill-column-mode 1))


(use-package adaptive-wrap)
(use-package visual-fill-column
  :defer t
  :hook (org-mode . em/org-mode-visual-fill))
```


### Ox-hugo {#ox-hugo}

```emacs-lisp
(use-package ox-hugo
  :config
  (setq org-hugo-front-matter-format "toml")
  :after ox)
```


### Task {#task}

```emacs-lisp
(setq org-todo-keywords
  '((sequence
     "TODO(t)" "STARTED(s)" "DONE(d)" "WAITING(w)" "CANCELLED(c)" )))


```


### Images {#images}

```emacs-lisp

(setq org-startup-with-inline-images t)
(add-hook 'org-babel-after-execute-hook
	  'org-redisplay-inline-images)

;; (defun shk-fix-inline-images ()
;;   (when org-inline-image-overlays
;;     (org-redisplay-inline-images)))

;; (with-eval-after-load 'org
;;   (add-hook 'org-babel-after-execute-hook 'shk-fix-inline-images))

;; (with-eval-after-load 'org
;;   (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images))

;;; Scrolling.
;; Good speed and allow scrolling through large images (pixel-scroll).
;; Note: Scroll lags when point must be moved but increasing the number
;;       of lines that point moves in pixel-scroll.el ruins large image
;;       scrolling. So unfortunately I think we'll just have to live with
;;       this.
(pixel-scroll-mode)
(setq pixel-dead-time 0) ; Never go back to the old scrolling behaviour.
(setq pixel-resolution-fine-flag t) ; Scroll by number of pixels instead of lines (t = frame-char-height pixels).
(setq mouse-wheel-scroll-amount '(1)) ; Distance in pixel-resolution to scroll each mouse wheel event.
(setq mouse-wheel-progressive-speed nil) ; Progressive speed is too fast for me.

```


### Org copy blocks {#org-copy-blocks}

Little snippet for copying org source block. Cannot differentiate between different blocks. Only work with self heading block.

```emacs-lisp
(defun org-copy-blocks ()
  (interactive)
  (let ((code ""))
    (save-restriction
      (org-narrow-to-subtree)
      (org-babel-map-src-blocks nil
    (setq code (concat code (org-no-properties body)))))
    (kill-new code)))

(define-key evil-window-map (kbd "C-y") 'org-copy-blocks)

```


## Completetion {#completetion}


### Preserver Mini buffer history with savehist-mode {#preserver-mini-buffer-history-with-savehist-mode}

```emacs-lisp
(use-package savehist
  :config
  (setq history-length 25)
  (savehist-mode 1))

;; Individual history elements can be configured separately
;;(put 'minibuffer-history 'history-length 25)
;;(put 'evil-ex-history 'history-length 50)
;;(put 'kill-ring 'history-length 25)
```


### vertico for compilation {#vertico-for-compilation}

```emacs-lisp
(use-package vertico
  :init
  (vertico-mode)

  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-f" . vertico-exit)
              :map minibuffer-local-map
              ("C-h" . minibuffer-backward-kill)
              ("M-h" . backward-kill-word))


  :custom
  (vertico-cycle t)
  (vertico-resize t)
  :custom-face
  (vertico-current ((t (:background "#3a3f5a"))))
  :init
  (vertico-mode))

;; (use-package vertico-posframe)
;; (vertico-posframe-mode 1)
;; (setq vertico-posframe-parameters
;;       '((left-fringe . 8)
;;         (right-fringe . 8)))

```


### Corfu + Cape {#corfu-plus-cape}

```emacs-lisp
(use-package corfu
  :init
  (global-corfu-mode)
  :config
  (setq corfu-auto t
        corfu-echo-documentation t
        corfu-scroll-margin 0
        corfu-count 8
        corfu-max-width 50
        corfu-min-width corfu-max-width
        corfu-auto-prefix 2)

  ;; Make Evil and Corfu play nice
  (evil-make-overriding-map corfu-map)
  (advice-add 'corfu--setup :after 'evil-normalize-keymaps)
  (advice-add 'corfu--teardown :after 'evil-normalize-keymaps)

  ;;(corfu-history-mode 1)
  (savehist-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history)

  (defun corfu-enable-always-in-minibuffer ()
    (setq-local corfu-auto nil)
    (corfu-mode 1))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)

  :general
  (:keymaps 'corfu-map
            :states 'insert
            "C-n" 'corfu-next
            "C-p" 'corfu-previous
            "C-j" 'corfu-next
            "C-k" 'corfu-previous
            "RET" 'corfu-complete
            "<escape>" 'corfu-quit
            ))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(use-package kind-icon
  :config
  (setq kind-icon-default-face 'corfu-default)
  (setq kind-icon-default-style '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.9 :scale 1))
  (setq kind-icon-blend-frac 0.08)
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  (add-hook 'counsel-load-theme #'(lambda () (interactive) (kind-icon-reset-cache)))
  (add-hook 'load-theme         #'(lambda () (interactive) (kind-icon-reset-cache))))
```

```emacs-lisp
(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-commit-predicate nil)   ;; Do not commit selected candidates on next input
  (corfu-quit-at-boundary t)     ;; Automatically quit at word boundary
  (corfu-quit-no-match t)        ;; Automatically quit if there is no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect-first nil)    ;; Disable candidate preselection
  (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  (corfu-scroll-margin 5)        ;; Use scroll margin
(corfu-atuo t)
(corfu-quit-no-match 'separator)
(corfu-auto-delay 0)
(corfu-auto-prefix 0)
(completion-styles '(basic))

  ;; TAB-and-Go customizations
  ;; Use TAB for cycling, default is `corfu-complete'.
  ;; :bind
  ;; (:map corfu-map
        ;; ("TAB" . corfu-next)
        ;; ([tab] . corfu-next)
        ;; ("S-TAB" . corfu-previous)
        ;; ([backtab] . corfu-previous))
  ;; You may want to enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
         ;; (shell-mode . corfu-mode)
         ;; (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since dabbrev can be used globally (M-/).
  :init
  (global-corfu-mode))


;; Add extensions
(use-package cape
  ;; Bind dedicated completion commands
  ;;:bind (("C-c p p" . completion-at-point) ;; capf
   ;;      ("C-c p t" . complete-tag)        ;; etags
    ;;     ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
     ;;    ("C-c p f" . cape-file)
      ;;   ("C-c p k" . cape-keyword)
       ;;  ("C-c p s" . cape-symbol)
        ;; ("C-c p a" . cape-abbrev)
        ;; ("C-c p i" . cape-ispell)
        ;; ("C-c p l" . cape-line)
        ;; ("C-c p w" . cape-dict))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
)

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-hook 'my-completion-ui-mode-hook
   	    (lambda ()
   	      (setq completion-in-region-function
   		    (kind-icon-enhance-completion
   		     completion-in-region-function))))
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


;; Optionally use the `orderless' completion style. See `+orderless-dispatch'
;; in the Consult wiki for an advanced Orderless style dispatcher.
;; Enable `partial-completion' for files to allow path expansion.
;; You may prefer to use `initials' instead of `partial-completion'.

;; Use dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

```


### orderless {#orderless}

```emacs-lisp
;; Use the `orderless' completion style. Additionally enable
;; `partial-completion' for file path expansion. `partial-completion' is
;; important for wildcard support. Multiple files can be opened at once
;; with `find-file' if you enter a wildcard. You may also give the
;; `initials' completion style a try.

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

```


### Consult {#consult}

Consult has a lot of functionality. You need to discover it userself and use it as you need. `consult-locate` will use the systemwide locate to locate files. More about locate can be found at linux man page.

```emacs-lisp
(defun em/get-project-root ()
  (when (fboundp 'projectile-project-root)
    (projectile-project-root)))

(use-package consult
  :straight t
  :demand t
  :bind (("C-s" . consult-line)
         ("C-M-l" . consult-imenu)
         ;; ("C-M-j" . persp-switch-to-buffer*)
         ("C-x b" . consult-project-buffer)
         ("C-x l" . consult-buffer)
         ("C-;" . consult-register)
         ("C-q" . consult-bookmark)
         ("C-M-j" . consult-buffer-other-window)
         ("M-s" . consult-ripgrep)
         ("C-x t" . consult-theme)
         ;; ("C-y" . consult-yank-pop)
         ;; ("M-y" . consult-yank-replace)
         :map minibuffer-local-map
         ("C-r" . consult-history))
  :custom
  (consult-project-root-function #'em/get-project-root)
  (completion-in-region-function #'consult-completion-in-region)
  :config
  (consult-preview-at-point-mode))
```


#### Consult-dir {#consult-dir}

```emacs-lisp
(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir-dired)
         ("C-x d" . consult-dir)
         ("C-x f" . consult-locate)
         :map vertico-map
         ("C-x d" . consult-dir)
         ("C-x C-f" . consult-dir-jump-file)))

```


### Marginalia replacement for ivy-rich {#marginalia-replacement-for-ivy-rich}

```emacs-lisp
(use-package marginalia
  :after vertico
  :straight t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))
```


### Completion Actions with Embark {#completion-actions-with-embark}

```emacs-lisp

(use-package embark
  :straight t
  :bind (("C-." . embark-act)
         :map minibuffer-local-map
         ("C-." . embark-act))
  :config

  ;; Show Embark actions via which-key
  (setq embark-action-indicator
        (lambda (map)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator))

;; (use-package embark-consult
;;   :straight '(embark-consult :host github
;;                              :repo "oantolin/embark"
;;                              :files ("embark-consult.el"))
;;   :after (embark consult)
;;   :demand t
;;   :hook
;;   (embark-collect-mode . embark-consult-preview-minor-mode))

```


### Company {#company}

I set `company-minimum-prefix-lenght` to 3 for not messing with my &lt;sh or &lt;el keybinding in org-mode. It doesn't if you use `smarter-tab-to-complete`. `company-box` for using icons in the completions.

Company is setup correctly with my workflow.

```emacs-lisp
(use-package company
  ;; :hook ((prog-mode LaTeX-mode latex-mode ess-r-mode) . company-mode)
  :bind
  (:map company-active-map
        ([tab] . smarter-tab-to-complete)
        ;; ("C-e" . company-complete-selection)
        ("C-j" . company-select-next)
        ("C-k" . company-select-previous)
        )

  :custom
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t)
  (company-require-match 'never)
  ;; Don't use company in the following modes
  (company-global-modes '(not shell-mode eaf-mode))
  ;; Trigger completion immediately.
  (company-idle-delay 0.0)
  ;; Number the candidates (use M-1, M-2 etc to select completions).
  (company-show-numbers t)

  :config
  (global-company-mode 1)


  (defun smarter-tab-to-complete ()
    "Try to `org-cycle', `yas-expand', and `yas-next-field' at current cursor position.

    If all failed, try to complete the common part with `company-complete-common'"
    (interactive)
    (when yas-minor-mode
      (let ((old-point (point))
            (old-tick (buffer-chars-modified-tick))
            (func-list
             (if (equal major-mode 'org-mode) '(org-cycle yas-expand yas-next-field)
               '(yas-expand yas-next-field))))
        (catch 'func-suceed
          (dolist (func func-list)
            (ignore-errors (call-interactively func))
            (unless (and (eq old-point (point))

                         (eq old-tick (buffer-chars-modified-tick)))
              (throw 'func-suceed t)))
          (company-complete-selection))))))

```

```emacs-lisp
(use-package company-box
  :if (display-graphic-p)
  :defines company-box-icons-all-the-icons
  :hook (company-mode . company-box-mode)
  :custom
  (company-box-backends-colors nil)
  :config
  (with-no-warnings
    ;; Prettify icons
    (defun my-company-box-icons--elisp (candidate)
      (when (derived-mode-p 'emacs-lisp-mode)
        (let ((sym (intern candidate)))
          (cond ((fboundp sym) 'Function)
                ((featurep sym) 'Module)
                ((facep sym) 'Color)
                ((boundp sym) 'Variable)
                ((symbolp sym) 'Text)
                (t . nil)))))
    (advice-add #'company-box-icons--elisp :override #'my-company-box-icons--elisp))

  (when (and (display-graphic-p)
             (require 'all-the-icons nil t))
    (declare-function all-the-icons-faicon 'all-the-icons)
    (declare-function all-the-icons-material 'all-the-icons)
    (declare-function all-the-icons-octicon 'all-the-icons)
    (setq company-box-icons-all-the-icons
          `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.8 :v-adjust -0.15))
            (Text . ,(all-the-icons-faicon "text-width" :height 0.8 :v-adjust -0.02))
            (Method . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
            (Function . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
            (Constructor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
            (Field . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
            (Variable . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
            (Class . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
            (Interface . ,(all-the-icons-material "share" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
            (Module . ,(all-the-icons-material "view_module" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
            (Property . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.02))
            (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.8 :v-adjust -0.15))
            (Value . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
            (Enum . ,(all-the-icons-material "storage" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
            (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.8 :v-adjust -0.15))
            (Snippet . ,(all-the-icons-material "format_align_center" :height 0.8 :v-adjust -0.15))
            (Color . ,(all-the-icons-material "palette" :height 0.8 :v-adjust -0.15))
            (File . ,(all-the-icons-faicon "file-o" :height 0.8 :v-adjust -0.02))
            (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.8 :v-adjust -0.15))
            (Folder . ,(all-the-icons-faicon "folder-open" :height 0.8 :v-adjust -0.02))
            (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15))
            (Constant . ,(all-the-icons-faicon "square-o" :height 0.8 :v-adjust -0.1))
            (Struct . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
            (Event . ,(all-the-icons-octicon "zap" :height 0.8 :v-adjust 0 :face 'all-the-icons-orange))
            (Operator . ,(all-the-icons-material "control_point" :height 0.8 :v-adjust -0.15))
            (TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.02))
            (Template . ,(all-the-icons-material "format_align_left" :height 0.8 :v-adjust -0.15)))
          company-box-icons-alist 'company-box-icons-all-the-icons)))

```


### Flycheck {#flycheck}

```emacs-lisp
(use-package flycheck
  :defer t
  :hook (after-init . global-flycheck-mode)
  :commands (flycheck-add-mode)
  :custom
  (flycheck-global-modes
   '(not outline-mode diff-mode shell-mode eshell-mode term-mode))
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-indication-mode (if (display-graphic-p) 'right-fringe 'right-margin))
  :init
  (if (display-graphic-p)
      (use-package flycheck-posframe
        :custom-face
        (flycheck-posframe-face ((t (:foreground ,(face-foreground 'success)))))
        (flycheck-posframe-info-face ((t (:foreground ,(face-foreground 'success)))))
        :hook (flycheck-mode . flycheck-posframe-mode)
        :custom
        (flycheck-posframe-position 'window-bottom-left-corner)
        (flycheck-posframe-border-width 3)
        (flycheck-posframe-inhibit-functions
         '((lambda (&rest _) (bound-and-true-p company-backend)))))
    (use-package flycheck-pos-tip
      :defines flycheck-pos-tip-timeout
      :hook (flycheck-mode . flycheck-pos-tip-mode)
      :custom (flycheck-pos-tip-timeout 30)))
  :config
  (use-package flycheck-popup-tip
    :hook (flycheck-mode . flycheck-popup-tip-mode))
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center))
  (when (executable-find "vale")
    (use-package flycheck-vale
      :config
      (flycheck-vale-setup)
      (flycheck-add-mode 'vale 'latex-mode))))


(use-package flycheck-inline)
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-inline-mode))
```


### Snippets {#snippets}

How to create new snippets? Invoke the command `yas-new-snippet` then create it. $0 means the end of the pointer. $1 first one, $2 second one and so on. For placeholder purposes use ${1:name}, ${2:title}.

```emacs-lisp
(use-package yasnippet
  :ensure t
  :hook ((lsp-mode . yas-minor-mode))
  :config
  (setq yas-snippet-dirs '("~/.config/emacs/snippets/"))
  (yas-global-mode 1)
  )

(use-package yasnippet-snippets)

```


## Language {#language}


### codium {#codium}

Codium  uses a binary that's not open source so don't use it.

```emacs-lisp
;; (straight-use-package '(codeium :type git :host github :repo "Exafunction/codeium.el"))
;; we recommend using use-package to organize your init.el
(use-package codeium
    ;; if you use straight
    :straight '(:type git :host github :repo "Exafunction/codeium.el")
    ;; otherwise, make sure that the codeium.el file is on load-path

    :init
    ;; use globally
    (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
    ;; or on a hook
    ;; (add-hook 'python-mode-hook
    ;;     (lambda ()
    ;;         (setq-local completion-at-point-functions '(codeium-completion-at-point))))

    ;; if you want multiple completion backends, use cape (https://github.com/minad/cape):
    ;; (add-hook 'python-mode-hook
    ;;     (lambda ()
    ;;         (setq-local completion-at-point-functions
    ;;             (list (cape-super-capf #'codeium-completion-at-point #'lsp-completion-at-point)))))
    ;; an async company-backend is coming soon!

    ;; codeium-completion-at-point is autoloaded, but you can
    ;; optionally set a timer, which might speed up things as the
    ;; codeium local language server takes ~0.2s to start up
    ;; (add-hook 'emacs-startup-hook
    ;;  (lambda () (run-with-timer 0.1 nil #'codeium-init)))

    :defer t
    :config
    (setq use-dialog-box nil) ;; do not use popup boxes

    ;; if you don't want to use customize to save the api-key
    ;; (setq codeium/metadata/api_key "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")

    ;; get codeium status in the modeline
    (setq codeium-mode-line-enable
        (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
    (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
    ;; alternatively for a more extensive mode-line
    ;; (add-to-list 'mode-line-format '(-50 "" codeium-mode-line) t)

    ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
    (setq codeium-api-enabled
        (lambda (api)
            (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
    ;; you can also set a config for a single buffer like this:
    ;; (add-hook 'python-mode-hook
    ;;     (lambda ()
    ;;         (setq-local codeium/editor_options/tab_size 4)))

    ;; You can overwrite all the codeium configs!
    ;; for example, we recommend limiting the string sent to codeium for better performance
    (defun my-codeium/document/text ()
        (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
    ;; if you change the text, you should also change the cursor_offset
    ;; warning: this is measured by UTF-8 encoded bytes
    (defun my-codeium/document/cursor_offset ()
        (codeium-utf8-byte-length
            (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
    (setq codeium/document/text 'my-codeium/document/text)
    (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset))
```

```emacs-lisp
(use-package fontawesome)
```


### eglot {#eglot}

Emacs now has built in a Language Server Protocol eglot.

```emacs-lisp
(add-hook web-mode-hook #'eglot-ensure)

 (add-to-list 'eglot-server-programs
             '(typescript-mode . ("typescript-language-server" "--stdio" )))
(add-to-list 'eglot-server-programs
             '(web-mode . ("typescript-language-server" "--stdio" )))

;; yasnippet with company
(add-hook 'eglot-managed-mode-hook (lambda ()
                                     (add-to-list 'company-backends
                                                  '(company-capf :with company-yasnippet))))

;;corfu has a built-in support for this maybe.
;; only need to install capef


(defun em/eglot-capf ()
  (setq-local completion-at-point-functions
              (list (cape-super-capf
                     #'eglot-completion-at-point
                     (cape-company-to-capf #'company-yasnippet)))))

(add-hook 'eglot-managed-mode-hook #'my/eglot-capf)


(use-package eglot
  :hook ((mhtml-mode-hook
         js-mode-hook
         css-mode-hook
         python-mode-hook
         typescript-mode-hook
         web-mode-hook) . eglot-ensure)
)
```


### Tree-sitter {#tree-sitter}

```emacs-lisp
(use-package tree-sitter
  :config
  (use-package tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)
```

```emacs-lisp
(use-package treesit-auto
  :config
  ;; (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))
```


### Auto format {#auto-format}

```emacs-lisp
(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1))
```


### Python {#python}

This pyvenv-mode 1 part is essential. This will automatically make the venv activate when the variable pyvenv-activate is changed.

So, now leverage .dir-locals.el to set that for my projects.

`((nil . ((pyvenv-activate . "~/repos/my_proj/.venv"))))`

Now when I open a file in my project, that virtual environment is activated!

```emacs-lisp
(use-package pipenv
  :hook (python-mode . pipenv-mode)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended))
```

```emacs-lisp
(setq python-indent-offset 4)
```


### Yaml {#yaml}

```emacs-lisp
(use-package yaml-mode
  :mode "\\.ya?ml\\'")
```


### Rust {#rust}

```emacs-lisp
(use-package rustic)
```


### Typescript {#typescript}

Tide is typescript code manager or something like that. It works great for emacs.

```emacs-lisp
(use-package tide :ensure t)
(use-package company :ensure t)
(use-package flycheck :ensure t)

  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (setq typescript-indent-level 2)
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (company-mode +1))

  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)

  ;; formats the buffer before saving
  (add-hook 'before-save-hook 'tide-format-before-save)

  (add-hook 'typescript-mode-hook #'setup-tide-mode)
```

Web mode for editing tsx files.

```emacs-lisp
(use-package web-mode)

  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode))))

  ;; enable typescript - tslint checker
  (flycheck-add-mode 'typescript-tslint 'web-mode)

```

Eglot for typescript project setup.

```emacs-lisp

;; I'm not sure why this is needed, but it throws an error if I remove it
(cl-defmethod project-root ((project (head eglot-project)))
  (cdr project))

(defun my-project-try-tsconfig-json (dir)
  (when-let* ((found (locate-dominating-file dir "tsconfig.json")))
    (cons 'eglot-project found)))

(add-hook 'project-find-functions
          'my-project-try-tsconfig-json nil nil)


```

```emacs-lisp
(use-package typescript-mode
  :after tree-sitter
  :init (setq typescript-indent-level 2)
  :config

  ;; we choose this instead of tsx-mode so that eglot can automatically figure out language for server
  ;; see https://github.com/joaotavora/eglot/issues/624 and https://github.com/joaotavora/eglot#handling-quirky-servers
  (define-derived-mode typescriptreact-mode typescript-mode
    "TypeScript")

  ;; use our derived mode for tsx files
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  ;; by default, typescript-mode is mapped to the treesitter typescript parser
  ;; use our derived mode to map both .tsx AND .ts -> typescriptreact-mode -> treesitter tsx
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))

;; (use-package tsi
;;   :after tree-sitter
;;   :straight (tsi.el :type git :host github :repo "orzechowskid/tsi.el")
;;   ;; define autoload definitions which when actually invoked will cause package to be loaded
;;   :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
;;   :init
;; (setq tsi-typescriptreact
;;   (add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
;;   (add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
;;   (add-hook 'css-mode-hook (lambda () (tsi-css-mode 1)))
;;   (add-hook 'scss-mode-hook (lambda () (tsi-scss-mode 1))))
```


### Prisma for Node database ORM. {#prisma-for-node-database-orm-dot}

```emacs-lisp

(use-package dotenv-mode)

(straight-use-package
 '(emacs-prisma-mode :type git :host github :repo "pimeys/emacs-prisma-mode"))
```


### JSON {#json}

```emacs-lisp
(use-package json-mode)
```


### Go mode {#go-mode}

```emacs-lisp
(use-package go-mode)
(defun lsp-go-install-save-hooks ()
  ;; (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t)
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
  )

(use-package go-projectile)
(use-package gotest)

(defun my-go-mode-hook ()
      (setq tab-width 2 indent-tabs-mode 1)
      ; eldoc shows the signature of the function at point in the status bar.
      (local-set-key (kbd "M-.") #'godef-jump)

      (let ((map go-mode-map))
        (define-key map (kbd "C-c a") 'go-test-current-project) ;; current package, really
        (define-key map (kbd "C-c m") 'go-test-current-file)
        (define-key map (kbd "C-c .") 'go-test-current-test)
        (define-key map (kbd "C-c C-c") 'go-run)))

 (let ((goimports (executable-find "goimports")))
      (when goimports
        (setq gofmt-command goimports)))

    ;; stop whitespace being highlighted
    (whitespace-toggle-options '(tabs))

    ;; CamelCase aware editing operations
    (subword-mode +1)

(add-hook 'go-mode-hook 'my-go-mode-hook)

```


### lorem-ipsum {#lorem-ipsum}

```emacs-lisp
(use-package lorem-ipsum)
(use-package emmet-mode)
(add-hook 'html-mode-hook 'emmet-mode)
```


### LSP Mode {#lsp-mode}

Install `python-language-server` from the arch package repository.

If you have problem with `json-string-format` check with the project package.json that all it's syntax is correct.

```emacs-lisp
(use-package lsp-mode
  :init
  (setq lsp-enable-snippet t)
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
         (js-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (web-mode . lsp-deferred)
         ;; if you want which-key integration
         (html-mode . lsp-deferred)
         (css-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp lsp-deferred)

;; ;; optionally
;; (use-package lsp-ui
;;   :init
;;   (setq lsp-ui-doc-enable t)
;;   (setq lsp-ui-doc-header t)
;;   (setq lsp-ui-doc-include-signature t)
;;   (setq lsp-ui-doc-max-width 120)
;;   (setq lsp-ui-doc-max-height 20)
;;   (setq lsp-ui-doc-delay 0.3)
;;   (setq lsp-ui-doc-use-childframe t)
;;   (setq lsp-ui-peek-enable t)
;;   (setq lsp-ui-peek-peek-height 20)
;;   (setq lsp-ui-peek-fontify 'on-demand)
;;   (setq lsp-ui-imenu-window-width 0)
;;   (setq lsp-ui-imenu-kind-position 'top)
;;   :commands lsp-ui-mode)


;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)


```


### Debug Adapter Support {#debug-adapter-support}

```emacs-lisp
(use-package dap-mode
  :straight t
  :custom
  (lsp-enable-dap-auto-configure nil)
  :config
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (require 'dap-node)
  (require 'dap-python)
  (require 'dap-cpptools)
  (dap-node-setup))

```


### Yuck {#yuck}

```emacs-lisp
(use-package yuck-mode)
```


### Sudo edit {#sudo-edit}

```emacs-lisp
(use-package crux
  :bind (("C-a" . crux-move-beginning-of-line)
         ("C-o" . crux-smart-open-line)
         ("C-S-o" . crux-smart-open-line-above)
         ))

```
