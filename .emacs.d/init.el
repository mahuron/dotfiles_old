;; set load-path
(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; start emacs server
(server-start)

;; general ui options
(setq inhibit-splash-screen t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; keyboard
;; mac modifiers need fixing
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;; selection options
(setq transient-mark-mode nil)
(setq mouse-yank-at-point t)

;; disable warnings for "advanced" functions
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; built in version control
(setq
 backup-by-copying t
 backup-directory-alist '(("." . "~/.saves"))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)

;; themes
(require 'color-theme)
(color-theme-initialize)
(color-theme-arjen)

;; shorten literal lambda to greek character
(require 'pretty-lambdada)
(pretty-lambda-for-modes)

;; magit
(require 'magit)

;; python
(add-hook 'python-mode-hook 'turn-on-pretty-lambda-mode)
