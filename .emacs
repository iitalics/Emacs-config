;;;; Bootstrap straight ;;;;

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

(let ((bootstrap-file (concat user-emacs-directory "straight/bootstrap.el"))
      (bootstrap-version 2))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(package-initialize)
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)


;;;; Paths ;;;;

(add-to-list 'backup-directory-alist '("." . "~/.emacs.d/backup-saves/"))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")


;;;; Font & Theme ;;;;

(custom-set-faces
 '(default
    ((t (:family "Source Code Pro"
         :foundry "ADBO"
         :slant normal :weight semi-bold :width normal
         :height 90)))))

(use-package font-lock+
  :straight (:type git
             :host github
             :repo "emacsmirror/font-lock-plus"
             :files ("font-lock+.el")))

(use-package doom-themes
  :config
  (load-theme 'doom-molokai t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; PACKAGES: General ;;;;

(use-package shell
  :bind (("M-s M-s" . shell)))

(use-package undo-tree
  :straight (:type git
             :host github
             :repo "emacsmirror/undo-tree"
             :files ("undo-tree.el")))

(use-package evil)
(evil-mode 1)

(use-package flx)
(use-package ivy
  :after (flx)
  :bind (("C-s" . swiper)
         ("C-x C-f" . counsel-find-file)
         :map ivy-minibuffer-map
         ("TAB" . ivy-partial)
         ("RET" . ivy-alt-done))
  :config
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy))))
(ivy-mode 1)

(use-package magit
  :bind (("M-g M-s" . magit-status)))


;;;; PACKAGES: Languages ;;;;

(use-package racket-mode)

(use-package tuareg
  :bind (:map tuareg-mode-map ("C-c C-c" . tuareg-eval-buffer)))

(use-package haskell-mode
  :bind (:map haskell-mode-map ("C-c C-l" . haskell-process-load-file))
  :config
  (setq haskell-process-type 'stack-ghci)
  (setq haskell-process-path-ghci "stack"))


;;;; PACKAGES: Autocomplete ;;;;

(use-package company
  :bind (:map company-mode-map ("<C-tab>" . company-complete))
  :config
  (add-to-list 'company-backends 'company-c-headers)
  (add-to-list 'company-backends 'company-racer)
  (global-company-mode 1))

(use-package flycheck
  :config
  (add-hook 'c++-mode-hook
            (lambda ()
              (setq-local flycheck-clang-language-standard "c++1z")
              (setq-local company-clang-arguments '("-std=c++1z"))))
  (dolist (hook '(c-mode-hook))
    (add-hook hook 'flycheck-mode)))

(use-package merlin
  :after (tuareg)
  :config (add-hook 'tuareg-mode-hook 'merlin-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Enable/disable modes ;;;;

(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)


;;;; Tabs and whitespace ;;;;

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
;(setq-default js-indent-level 2)
;(setq-default rust-indent-offset 4)


;;;; Misc. variables ;;;;

(setq inhibit-startup-screen t)
(setq initial-major-mode 'racket-mode)


;;;; Misc. hooks ;;;;

(setq del-trailing t)
(add-hook 'before-save-hook
	  (lambda ()
	    (when del-trailing
	      (delete-trailing-whitespace))))
