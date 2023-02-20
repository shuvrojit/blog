+++
title = "My Emacs Configuration"
author = ["Shuvrojit Biswas"]
date = 2023-02-20
lastmod = 2023-02-20T23:42:01+06:00
tags = ["emacs", "org"]
categories = ["emacs"]
draft = false
weight = 2001
important = true
+++

The editor of lengends. is hackable via  Programming Language.


## Startup {#startup}


### Measure startup time {#measure-startup-time}

Garbage collection is a technique used in computer programming to automatically manage the allocation and release of memory. It involves tracking the objects that are no longer in use by the program and freeing up the memory they occupied. The goal of garbage collection is to reduce the amount of manual memory management required by the programmer and to prevent memory leaks, which can lead to performance problems.

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

```


### Native Compilation {#native-compilation}

```emacs-lisp

;; Silence compiler warnings as they can be pretty disruptive
(setq comp-async-report-warnings-errors nil)

```

No idea what this is. Something to do with native compilation.

```emacs-lisp

(when (fboundp 'native-compile-async)
  (setq comp-deferred-compilation t
        comp-deferred-compilation-black-list '("/mu4e.*\\.el$")))

```


### Emacs Environment  variable {#emacs-environment-variable}

You can set emacs environment variable like this.

```emacs-lisp

(setq debug-on-error t)
(setenv "PATH" (concat (getenv "PATH") "/home/shuvro/.local/bin"))
(setq exec-path (append exec-path '("/home/shuvro/.local/bin")))

(setq user-full-name    "Shuvrojit Biswas"
      user-mail-address "jsuvro17@gmail.com")

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

<a id="figure--emacs"></a>

{{< figure src="/ox-hugo/emacs.png" caption="<span class=\"figure-number\">Figure 1: </span>This is the caption for the emacs figure link" >}}


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


### Modus Theme {#modus-theme}

```emacs-lisp
(use-package modus-themes
  :ensure
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-region '(accented bg-only)
        modus-themes-modeline '(accented borderless padded)
        modus-themes-paren-match '(bold intense)
        modus-themes-markup '(bold underline)
        modus-themes-org-blocks '(tinted-background)
        modus-themes-headings
        '((1 . (background overline variable-pitch 1.5))
          (2 . (overline rainbow 1.3))
          (3 . (overline 1.1))
          (t . (monochrome)))
        )

  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)
  :config
  (modus-themes-load-vivendi)
  :bind ("<f5>" . modus-themes-toggle))

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


### Solarized Theme+Modeline {#solarized-theme-plus-modeline}

```emacs-lisp
;;; module-solarized.el --- solarized module for my emacs

;; Author: Mark Feller <mark.feller@member.fsf.org>

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(use-package solarized-theme
  :config
  (progn (setq solarized-emphasize-indicators nil
               solarized-high-contrast-mode-line nil
               solarized-scale-org-headlines nil
               solarized-use-less-bold t
               solarized-use-variable-pitch nil
               solarized-distinct-fringe-background nil)))

(use-package all-the-icons
  :demand
  :init
  (progn (defun -custom-modeline-github-vc ()
           (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
             (concat
              (propertize (format " %s" (all-the-icons-octicon "git-branch"))
                          'face `(:height 1 :family ,(all-the-icons-octicon-family))
                          'display '(raise 0))
              (propertize (format " %s" branch))
              (propertize "  "))))

         (defun -custom-modeline-svn-vc ()
           (let ((revision (cadr (split-string vc-mode "-"))))
             (concat
              (propertize (format " %s" (all-the-icons-faicon "cloud"))
                          'face `(:height 1)
                          'display '(raise 0))
              (propertize (format " %s" revision) 'face `(:height 0.9)))))

         (defvar mode-line-my-vc
           '(:propertize
             (:eval (when vc-mode
                      (cond
                       ((string-match "Git[:-]" vc-mode) (-custom-modeline-github-vc))
                       ((string-match "SVN-" vc-mode) (-custom-modeline-svn-vc))
                       (t (format "%s" vc-mode)))))
             face mode-line-directory)
           "Formats the current directory."))
  :config
  (progn
    ;; (setq-default mode-line-format
    ;;               (list
    ;;                evil-mode-line-tag
    ;;                mode-line-front-space
    ;;                mode-line-mule-info
    ;;                mode-line-modified
    ;;                mode-line-frame-identification
    ;;                mode-line-buffer-identification
    ;;                " "
    ;;                mode-line-position
    ;;                mode-line-my-vc
    ;;                mode-line-modes))
    ;; (concat evil-mode-line-tag)
    ))


;; (bind-keys ("C-c tl" . (lambda () (interactive) (load-theme 'solarized-light)))
;;            ("C-c td" . (lambda () (interactive) (load-theme 'solarized-dark))))

(load-theme 'solarized-light t)

(set-face-attribute 'mode-line nil
                    :background "#eee8d5"
                    :foreground "#657b83"
                    :box '(:line-width 4 :color "#eee8d5")
                    :overline nil
                    :underline nil)

(set-face-attribute 'mode-line-inactive nil
                    :background "#fdf6e3"
                    :foreground "#93a1a1"
                    :box '(:line-width 4 :color "#eee8d5")
                    :overline nil
                    :underline nil)

(define-minor-mode minor-mode-blackout-mode
  "Hides minor modes from the mode line."
  t)

(catch 'done
  (mapc (lambda (x)
          (when (and (consp x)
                     (equal (cadr x) '("" minor-mode-alist)))
            (let ((original (copy-sequence x)))
              (setcar x 'minor-mode-blackout-mode)
              (setcdr x (list "" original)))
            (throw 'done t)))
        mode-line-modes))

(global-set-key (kbd "C-c m") 'minor-mode-blackout-mode)
;; ;; window dividers
;; (window-divider-mode t)
;; (setq window-divider-default-right-width 2)

;; (set-face-attribute 'window-divider nil :foreground "#eee8d5")
;; (set-face-attribute 'window-divider-first-pixel nil :foreground "#eee8d5")
;; (set-face-attribute 'window-divider-last-pixel nil :foreground "#eee8d5")

(provide 'module-solarized)

;;; module-solarized.el ends here

```


### Ewal {#ewal}

Based on pywal. You can find more about  pywal in.

```emacs-lisp
(use-package ewal
  :init (setq ewal-use-built-in-always-p nil
              ewal-use-built-in-on-failure-p t
              ewal-built-in-palette "sexy-material"))
(use-package ewal-spacemacs-themes
  :init (progn
          (setq spacemacs-theme-underline-parens t
                my:rice:font (font-spec
                              :family "Source Code Pro"
                              :weight 'semi-bold
                              :size 11.0))
          (show-paren-mode +1)
          (global-hl-line-mode)
          (set-frame-font my:rice:font nil t)
          (add-to-list  'default-frame-alist
                        `(font . ,(font-xlfd-name my:rice:font))))
  :config (progn
            (load-theme 'ewal-spacemacs-modern t)
            (enable-theme 'ewal-spacemacs-modern)))
(use-package ewal-evil-cursors
  :after (ewal-spacemacs-themes)
  :config (ewal-evil-cursors-get-colors
           :apply t :spaceline t))
(use-package spaceline
  :after (ewal-evil-cursors winum)
  :init (setq powerline-default-separator nil)
  :config (spaceline-spacemacs-theme))
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


### Surround Vim {#surround-vim}

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
         python-mode))

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


### Ag {#ag}

```emacs-lisp
(use-package ag)
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
  (persp-mode)
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))
  :config
  ;; In the modeline, tell me which workspace I'm in.
  (persp-turn-on-modestring))
```

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


### Window History with winner-mode {#window-history-with-winner-mode}

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
