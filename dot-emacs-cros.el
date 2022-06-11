;; -*- emacs-lisp -*-
;; Keunwoo Lee's .emacs file for ChromeOS

;;;;;;;;;;;;;;;;;;;;;; PRELIMINARIES ;;;;;;;;;;;;;;;;;;;;

;; Added by Package.el.  This must come before configurations of installed packages.
(if (fboundp 'package-initialize)
    (package-initialize))

;; Custom load paths
(cd (getenv "HOME"))
(add-to-list 'load-path (concat (getenv "HOME") "/lib/emacs"))

(require 'dot-emacs-common)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(c-offsets-alist '((innamespace . +)))
 '(column-number-mode t)
 '(dired-use-ls-dired nil)
 '(elisp-cache-byte-compile-files t)
 '(explicit-shell-file-name "/bin/bash")
 '(grep-command "grep -nHi ")
 '(ibuffer-enable t)
 '(ibuffer-formats
   '((mark modified read-only " "
           (name 32 32 :left :elide)
           " "
           (size 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " " filename-and-process)
     (mark " "
           (name 16 -1)
           " " filename)))
 '(json-reformat:indent-width 2)
 '(longlines-show-hard-newlines nil)
 '(longlines-wrap-follows-window-size t)
 '(magit-refs-sections-hook
   '(magit-insert-error-header magit-insert-branch-description magit-insert-local-branches))
 '(octave-block-offset 4)
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(adaptive-wrap column-enforce-mode company flycheck-flow git-commit go-mode helm-ls-git js2-mode json-mode jsx-mode magit markdown-mode prettier-js rust-mode swift-mode tide urlenc use-package web-mode))
 '(ps-print-header-frame nil)
 '(safe-local-variable-values
   '((eval rename-buffer "*notes*")
     (buffer-file-coding-system . utf-8-dos)
     (css-indent-offset . 2)))
 '(scroll-bar-mode 'right)
 '(show-trailing-whitespace t)
 '(tide-sync-request-timeout 5)
 '(tide-tsserver-process-environment '("NODE_OPTIONS='--max_old_space_size=8000'"))
 '(tool-bar-mode nil)
 '(vc-follow-symlinks nil)
 '(visible-bell t)
 '(visible-cursor nil)
 '(web-mode-attr-indent-offset 4)
 '(web-mode-enable-optional-tags nil)
 '(web-mode-script-padding 0)
 '(web-mode-style-padding 4)
 '(whitespace-style
   '(face tabs trailing space-before-tab empty space-after-tab tab-mark)))

(setq tide-node-executable (concat (getenv "HOME") "/bin/node-activated"))
(setq tide-tscompiler-executable (concat (getenv "HOME") "/lib/typescript/node_modules/typescript/bin/tsserver"))
(setq tide-tsserver-executable (concat (getenv "HOME") "/lib/typescript/node_modules/typescript/bin/tsserver"))

(setenv "DENO_INSTALL" (concat (getenv "HOME") "/.deno"))

(custom-set-faces
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "GOOG" :family "Noto Sans Mono")))))
