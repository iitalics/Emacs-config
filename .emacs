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
;(setq use-package-verbose t)


;;;; Paths ;;;;

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(add-to-list 'backup-directory-alist '("." . "~/.emacs.d/backup-saves/"))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")


;;;; Elisp ;;;;

(use-package cl-lib
  :demand)

(defmacro λ (args &rest body)
  `(lambda ,args ,@body))


;;;; Font & Theme ;;;;

(custom-set-faces
 '(default
    ((t (:family "Source Code Pro"
         :foundry "ADBO"
         :weight semi-bold
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

(require 'quail)
(use-package diminish)
(use-package flx)
(use-package s)

(use-package undo-tree
  :straight (:host github
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
  :bind (("M-g M-s" . magit-status))
  :config
  (setq magit-log-section-commit-count 4))

(use-package dr-racket-like-unicode
  :bind (("C-c C-\\" . dr-racket-like-unicode-char))
  :config
  (let ((custom-table
         '(("\\composition" . "○")
           ("\\varphi" . "\u03c6")
           ("\\ell" . "\u2113")
           ("\\ok" . "\u2713")
           ("\\notok" . "\u2717"))))
    (setq dr-racket-like-unicode-table
          (cl-union custom-table
                    dr-racket-like-unicode-table))))

(use-package which-key)
(which-key-mode 1)


;;;; PACKAGES: Languages ;;;;

(use-package yaml-mode)
(use-package toml-mode)
(use-package rust-mode)
(use-package markdown-mode)

(use-package racket-mode
  :straight nil
  :load-path "/home/milo/Git/racket-mode/"
  :config
  (put 'reduction-relation 'racket-indent-function 1)
  (put 'test--> 'racket-indent-function 1))

(use-package tuareg
  :bind (:map tuareg-mode-map ("C-c C-c" . tuareg-eval-buffer)))

(defun tuareg-abbrev-hook ()
  (interactive) ())

(use-package haskell-mode
  :bind (:map haskell-mode-map ("C-c C-l" . haskell-process-load-file))
  :config
  (setq haskell-font-lock-symbols t
        haskell-process-type 'stack-ghci
        haskell-process-path-ghci "stack"
        haskell-font-lock-symbols-alist
        '(("\\" . "λ") ("::" . "∷") ("forall" . "∀")
                                        ; ("not" . "¬")
          ("." "○" haskell-font-lock-dot-is-not-composition)
          ("->" . "→") ("<-" . "←") ("=>" . "⇒")
          ("==" . "≡") ("/=" . "≢") (">=" . "≥") ("<=" . "≤")
          ("!!" . "‼") ("&&" . "∧") ("||" . "∨"))))

(use-package idris-mode
  :config (add-hook 'idris-mode-hook (λ () (setq-local tab-width 2))))

(defvar tsu--agda-mode-dir
  (file-name-directory
   (shell-command-to-string "agda-mode locate")))

(use-package agda2
  :straight nil
  :load-path tsu--agda-mode-dir)

(use-package meghanada
  :config (add-hook 'java-mode-hook
                    (λ ()
                       (meghanada-mode t))))

(use-package tex-mode
  :bind (:map latex-mode-map ("C-c C-]" . #'latex-close-block)))


;;;; PACKAGES: Autocomplete ;;;;

(use-package company-c-headers)

(use-package company
  :init (global-company-mode 1)
  :bind (:map company-mode-map ("<C-tab>" . company-complete))
  :config
  (setq company-global-modes '(not racket-mode
                                   racket-repl-mode))
  (add-to-list 'company-backends 'company-c-headers)
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

(use-package racer
  :config
  (add-hook 'rust-mode-hook 'racer-mode)
  (add-hook 'racer-mode-hook 'eldoc-mode)
                                        ;(add-to-list 'company-backends 'company-racer)
  )

(use-package rustfmt
  :preface
  (defun tsu-rust-fmt-and-save ()
    (interactive)
    (rustfmt-format-buffer)
    (save-buffer))
  :bind (:map rust-mode-map ("C-x M-s" . tsu-rust-fmt-and-save)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Disable dumb stuff ;;;;

(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(setq visible-bell nil)

;;;; Tabs and whitespace ;;;;

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
;(setq-default js-indent-level 2)
;(setq-default rust-indent-offset 4)


;;;; Misc. variables ;;;;

(defvar tsu-delete-trailing t)
(setq inhibit-startup-screen t)
(setq initial-major-mode 'racket-mode)
(load "~/.emacs.d/initial-scratch-msg.el")


;;;; Misc. hooks ;;;;

(add-hook 'before-save-hook
          (λ ()
             (when tsu-delete-trailing
               (delete-trailing-whitespace))))


;;;; Misc. keys ;;;;

(global-unset-key (kbd "C-x C-b"))
(global-set-key (kbd "C-x C-b") #'switch-to-buffer)
(global-set-key (kbd "C-c <C-return>") 'compile)


(defun tsu--dir-contains-opam-p (dir)
  (let ((opam-found nil))
    (dolist (f (directory-files dir))
      (when (and (string-suffix-p ".opam" f)
                 (not (string-equal f ".opam")))
        (setq opam-found t)))
    opam-found))

(defun tsu--find-opam-dir (dir depth)
  (cond
   ((string-equal dir "/") nil)
   ((<= depth 0) nil)
   ((tsu--dir-contains-opam-p dir) dir)
   (t (let* ((parent-dir (file-name-directory dir))
             (parent-dir* (string-remove-suffix "/" parent-dir)))
        (tsu--find-opam-dir parent-dir* (1- depth))))))
