;; set load-path
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/lisp/magit")
(add-to-list 'load-path "~/.emacs.d/lisp/color-theme")
(add-to-list 'load-path "~/.emacs.d/lisp/python-mode")

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
(add-to-list 'pretty-lambda-auto-modes 'python-mode)
(pretty-lambda-for-modes)

;; magit
(require 'magit)

;; whitespace mode
(setq whitespace-style '(face tabs trailing lines space-before-tab space-after-tab empty))

;; python
(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(setq py-install-directory "~/.emacs.d/lisp/python-mode")
(setq py-shell-name "ipython")

;; lisp
(add-hook 'emacs-lisp-mode-hook '(lambda () (setq indent-tabs-mode 'nil)))

;; c
(defconst f5-c-style
  '((c-basic-offset             .  4)
    (c-comment-only-line-offset .  (0 . 0))
    (c-offsets-alist            .  ((statement-block-intro . +)
                                    (knr-argdecl-intro     . 5)
                                    (substatement-open     . 0)
                                    (label                 . 0)
                                    (statement-case-open   . +)
                                    (statement-cont        . +)
                                    (arglist-intro . c-lineup-arglist-intro-after-paren)
                                    (arglist-close . c-lineup-arglist)
                                    (inline-open           . 0)
                                    ))
    (c-special-indent-hook       .  c-gnu-impose-minimum)
    (c-comment-continuation-stars . "")
    (c-hanging-comment-ender-p    . t)
   )
  "f5-c-style")

(defun f5-c-mode-common-hook ()
  (c-add-style "f5-c-style" f5-c-style t)
  (setq indent-tabs-mode 'nil)
  (setq tab-width 4)
  (c-toggle-auto-hungry-state 1)
  (whitespace-mode)
)

(add-hook 'c-mode-common-hook 'f5-c-mode-common-hook)
