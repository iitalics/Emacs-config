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

(defmacro λ (args &rest body)
  `(lambda ,args ,@body))


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

(use-package ample-theme)
(enable-theme 'ample)
(custom-theme-set-faces
 'ample
 '(minibuffer-prompt ((t (:foreground "#528fd1" :bold t :background nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; PACKAGES: General ;;;;

(use-package diminish)
(use-package flx)

(use-package undo-tree
  :straight (:type git
             :host github
             :repo "emacsmirror/undo-tree"
             :files ("undo-tree.el"))
  :config
  (diminish 'undo-tree-mode))

(use-package evil)
(evil-mode 1)

(use-package ivy
  :bind (("C-s" . swiper)
         ("C-x C-f" . counsel-find-file)
         :map ivy-minibuffer-map
         ("TAB" . ivy-partial)
         ("RET" . ivy-alt-done))
  :config
  (setq ivy-re-builders-alist
        '((swiper . regexp-quote)
          (t . ivy--regex-fuzzy)))
  (diminish 'ivy-mode))
(ivy-mode 1)

(use-package shell
  :bind (("M-s M-s" . shell)))

(use-package magit
  :bind (("M-g M-s" . magit-status)))

(use-package dr-racket-like-unicode
  :bind (("C-c C-\\" . dr-racket-like-unicode-char)))


;;;; PACKAGES: Languages ;;;;

(use-package racket-mode)
(use-package python-mode)
(use-package idris-mode)

(use-package tuareg
  :bind (:map tuareg-mode-map ("C-c C-c" . tuareg-eval-buffer)))

(use-package haskell-mode
  :bind (:map haskell-mode-map ("C-c C-l" . haskell-process-load-file))
  :config
  (setq haskell-font-lock-symbols t)
  (setq haskell-font-lock-symbols-alist
        '(("\\" . "λ") ("::" . "∷") ("forall" . "∀")
          ("not" . "¬") ("." "○" haskell-font-lock-dot-is-not-composition)
          ("->" . "→") ("<-" . "←") ("=>" . "⇒")
          ("==" . "≡") ("/=" . "≢") (">=" . "≥") ("<=" . "≤")
          ("!!" . "‼") ("&&" . "∧") ("||" . "∨")))
  (setq haskell-process-type 'stack-ghci)
  (setq haskell-process-path-ghci "stack"))


;;;; PACKAGES: Autocomplete ;;;;

(use-package company
  :init (global-company-mode 1)
  :bind (:map company-mode-map ("<C-tab>" . company-complete))
  :config
  (add-to-list 'company-backends 'company-c-headers)
  (add-to-list 'company-backends 'company-racer)
  (diminish 'company-mode))

(use-package flycheck
  :config
  (add-hook 'c++-mode-hook
            (λ ()
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
	  (λ ()
	    (when del-trailing
	      (delete-trailing-whitespace))))
