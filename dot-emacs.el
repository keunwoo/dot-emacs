;; -*- emacs-lisp -*-
;; Keunwoo Lee's .emacs file.
;; Updated for GNU Emacs 24.5.1

;;;;;;;;;;;;;;;;;;;;;; PRELIMINARIES ;;;;;;;;;;;;;;;;;;;;

;; Custom load paths

;; Added by Package.el.  This must come before configurations of installed packages.
(if (fboundp 'package-initialize)
    (package-initialize))

(cd (getenv "HOME"))
(add-to-list 'load-path (concat (getenv "HOME") "/lib/emacs"))
(add-to-list 'load-path (concat (getenv "HOME") "/lib/site-emacs"))

;;;;;;;;;;;;;;;;;;;;;;; EDITING ;;;;;;;;;;;;;;;;;;;;;;;

;; TODO(keunwoo): figure out why I need this; without it we get:
;; Symbol's function definition is void: "eieio-build-class-alist"
;; (require 'eieio-opt)

;; Get tabs the way I want
(setq-default indent-tabs-mode nil)

;; The One True Indent Style
(setq-default c-basic-offset 4)
(setq c-default-style "K&R")

;; I know how to use CVS, I don't need Emacs telling me what to do
(setq-default vc-handle-cvs nil)

;; I hate waiting for comint (command completion) to load the first
;; time I use it.
(autoload 'comint "comint-dynamic-complete")

;; Don't echo passwords for M-x ssh (and other shell modes?)
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

;; Enable narrow-to-region
(put 'narrow-to-region 'disabled nil)

;; Enable arbitrarily many recursive minibuffer sessions
(setq minibuffer-max-depth nil)

;; I usually write in US English.
(setq-default ispell-dictionary "american")
(setq-default ispell-local-dictionary "american")
;; Note: Setting ispell-local-dictionary above ought to be unnecessary.
;; Setting ispell-dictionary should be sufficient.  However, Debian
;; (for no good reason) fucks around with ispell.el in a way that
;; resets ispell-dictionary to "german-new" (or whatever your system
;; default is) every time you run M-x ispell-buffer.  I begin to share
;; Ben Laurie's opinion of vendors.

;; Configure helm.
;;
;; I assume the emacs maintainers had a good reason for making require
;; not work during init scripts, but it seems annoying.
(add-hook 'after-init-hook
          (lambda ()
            (require 'helm nil t)
            (require 'helm-config nil t)
            (require 'helm-ls-git nil t)
            ;; (helm-mode 1)  ; Not ready for this yet
            ;; Reset the insane default prefix.
            (global-set-key (kbd "C-c C-h") 'helm-command-prefix)
            (global-unset-key (kbd "C-x c"))
            ;; Find within current repo using helm.
            (global-set-key (kbd "C-c f") 'helm-ls-git-ls)))

;;; Configure git
(add-hook 'after-init-hook
          (lambda ()
            (require 'magit nil t)
            (global-set-key (kbd "C-x g") 'magit-status)
            ))

;;;;;;;;;;;;;;;;;;;;;;; DISPLAY ;;;;;;;;;;;;;;;;;;;;;;;

;; That splash screen is idiotic.
(setq inhibit-splash-screen t)

;; The "visible bell" in Emacs for OS X is a fucking piece of garbage.
;; https://www.reddit.com/r/emacs/comments/3omsr2/weird_display_issue_in_os_x/
(setq ring-bell-function (lambda () (message "*woop*")))

;; Turn font lock on for all modes
(global-font-lock-mode 't)

;; Or a toolbar?
(if (functionp 'tool-bar-mode)
    (tool-bar-mode -1))

;; Or a menubar? (except on OS X, where the menubar is always there anyway)
(if (not (eq window-system 'ns))
    (menu-bar-mode -1))

;; Or a #@&$!&@ blinking cursor?
(blink-cursor-mode nil)

;; Start gnuserv (enables inverse search in KDVI)
;;(gnuserv-start)

; Save faces with all other options
(setq options-save-faces t)

; Underlining sucks.
(setq face-underline-p nil)

; Always show line and col no.
(line-number-mode 1)
(column-number-mode 1)

;; (require 'column-marker)
(add-hook 'java-mode-hook
          '(lambda ()
             ; (font-lock-set-up-width-warning 100)
             (c-set-offset 'arglist-intro '++)
             (c-set-offset 'arglist-cont 0)
             (c-set-offset 'arglist-cont-nonempty '++)))

;; Rotates among custom background colors
(defvar background-color-rotation
  '("white" "aliceblue" "thistle1" "lemonchiffon" "khaki" "papayawhip"
    "honeydew" "mistyrose" "pale turquoise")
  "List of background color names for next-background-color to rotate.")
(defun next-background-color ()
  "Returns successive background colors named in background-color-rotation."
  (set-variable 'background-color-rotation
                (append (cdr background-color-rotation)
                        (list (car background-color-rotation))))
  (car background-color-rotation))
(defun rotate-background-color ()
  "Sets current background to next background color>"
  (set-face-background 'default (next-background-color)))

;; Rotates among some standardized frame widths.
(defvar frame-width-rotation
  ;; preferred column widths for python/js/C++, rust, java, go, Airtable js
  ;; (well, go & Airtable don't have a recommended width, but 120/130 fits most code)
  '(80 99 100 120 130)
  "List of column widths to rotate.")
(defun frame-width-next (frame)
  "Returns next width for frame in frame-width-rotation."
  (let* ((current-width (frame-width (selected-frame)))
         (next-width
          (seq-some (lambda (width) (if (> width current-width) width nil))
                    frame-width-rotation)))
    (if (eq next-width nil)
        (car frame-width-rotation)
      next-width)))
(defun frame-width-rotate ()
  "Sets current frame width to next frame width>"
  (let ((f (selected-frame)))
    (set-frame-width f (frame-width-next f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLIPBOARD

;; On rare occasions when I use the menu bar, I don't need the edit
;; menu to duplicate yank/kill; use X11 clipboard instead.
(menu-bar-enable-clipboard)

;; C-y does ordinary yank, so bind Ctrl-insert, Shift-insert, and
;; Shift-delete to copy, paste, and cut from X11 clipboard.
(define-key global-map [S-insert] 'clipboard-yank)
(define-key global-map [C-insert] 'clipboard-kill-ring-save)
(define-key global-map [S-delete] 'clipboard-kill-region)

;;;;;;;;;;;;;;;;;;;;;;; KEY MAPS ;;;;;;;;;;;;;;;;;;;;;;;

;; I don't know why one would set this any other way, since C-d does
;; forward delete... whatever.
(setq delete-key-deletes-forward nil)

;; Convenient compilation macro; this key combo is used on very few
;; modes, and happens to be the same keystroke used to view TeX output
(define-key global-map "\C-c\C-v" 'compile)
(define-key global-map "\C-c\C-m" 'compile)

;; Enable recursive minibuffers
(set-variable 'enable-recursive-minibuffers 't)

;; man page lookup (by default, f1 is help, but I already know how to
;; bring that up using C-h)
(define-key global-map [f1]
  (lambda () (interactive) (manual-entry (current-word))))

;; F2 to spawn another frame
(define-key global-map [f2] (lambda () (interactive) (make-frame)))

;; F3 to kill the other window
(define-key global-map [f3] (lambda () (interactive) (delete-other-windows)))

;; F4 for dired buffer of the current directory in the other window
(define-key global-map [f4]
  (lambda () (interactive) (dired-other-window default-directory)))

;; F5 to quickly call *scratch* in the other window
(define-key global-map [f5]
  (lambda () (interactive) (switch-to-buffer-other-window "*scratch*")))

;; F6 to rename the current buffer
(define-key global-map [f6]
  (lambda () (interactive)
    (rename-buffer
     (read-from-minibuffer
      "Rename current buffer: " ; Prompt in minibuffer
      (buffer-name)             ; Use current name initially
      ))))

;; F7 to rotate frame width.
(define-key global-map [f7]
  (lambda () (interactive) (frame-width-rotate)))

;; F8 to open dired buffer of the current directory, without dotfiles
(define-key global-map [f8]
  (lambda () (interactive)
    (dired-other-window (concat default-directory "[^.]*"))))

;; F9 to re-execute last compilation
(define-key global-map [f9]
  (lambda () (interactive) (recompile)))

;; ibuffer is better than the normal buffer list.
(define-key global-map "\C-x\C-b" 'ibuffer)

;; PrntScrn to invoke ps-print and generate an output PostScript file.
;;(require 'ps-print)
;;(define-key global-map [print]
;;  (lambda ()
;;    (interactive)
;;    (setq ps-print-color-p t)
;;    (ps-print-buffer-with-faces
;;     (read-string "PS output filename: "))))

;; Handy incremental scrolling keys
(define-key global-map "\M-n" (lambda () (interactive) (scroll-up 1)))
(define-key global-map "\M-p" (lambda () (interactive) (scroll-down 1)))

;; Make alt behave like Meta for all the commands I use frequently.
;; Handy when using a single instance of Emacs simultaneously on
;; multiple X servers that disagree on how to treat alt/meta.  And,
;; actually, this has the side benefit that if I accidentally hit
;; alt-k for some k that I never use, it's probably an accident, and
;; nothing will happen.
(define-key global-map [(alt a)] 'backward-sentence)
(define-key global-map [(alt b)] 'backward-word)
(define-key global-map [(alt c)] 'capitalize-word)
(define-key global-map [(alt d)] 'kill-word)
(define-key global-map [(alt e)] 'forward-sentence)
(define-key global-map [(alt f)] 'forward-word)
(define-key global-map [(alt l)] 'downcase-word)
(define-key global-map [(alt n)] (lambda () (interactive) (scroll-up 1)))
(define-key global-map [(alt p)] (lambda () (interactive) (scroll-down 1)))
(define-key global-map [(alt q)] 'fill-paragraph-or-region)
(define-key global-map [(alt t)] 'transpose-words)
(define-key global-map [(alt u)] 'upcase-word)
(define-key global-map [(alt v)] 'scroll-down)
(define-key global-map [(alt w)] 'kill-ring-save)
(define-key global-map [(alt y)] 'yank-pop)
(define-key global-map [(alt x)] 'execute-extended-command)
(define-key global-map [(alt y)] 'yank-pop)
(define-key global-map [(alt !)] 'shell-command)
(define-key global-map [(alt <)] 'beginning-of-buffer)
(define-key global-map [(alt >)] 'end-of-buffer)
(define-key global-map [(alt {)] 'backward-paragraph)
(define-key global-map [(alt })] 'forward-paragraph)
(define-key global-map [(alt ~)] 'not-modified)
(define-key global-map [(alt /)] 'dabbrev-expand)

;;;;;;;;;;;;;;;;;;;;;;; MODE HOOKS ETC ;;;;;;;;;;;;;;;;;;;;;;;

;; Mode for soft wrap.
(if (or (< emacs-major-version 24)
        (and (eq emacs-major-version 24) (< emacs-minor-version 4)))
    (require 'longlines))

;; todoo-mode is terrible.
(add-to-list 'auto-mode-alist '("TODO$" . fundamental-mode))

;;; go-mode
;; Need to update these...
;;(setq gofmt-command "goimports")
;;(add-hook 'go-mode-hook
;;          (lambda () (interactive)
;;            (column-marker-1 100)))
(defun auto-complete-for-go () (auto-complete-mode 1))
(add-hook 'after-init-hook
          (lambda ()
            (if (require 'go-mode nil t)
              (progn
                (setq gofmt-command "goimports")

                ;; Try to setup autocomplete.
                (if (require 'go-autocomplete nil t)
                    (add-hook 'go-mode-hook 'auto-complete-for-go))

                (add-hook 'before-save-hook 'gofmt-before-save)
                ))))

;; rust-mode
(add-hook 'rust-mode-hook 
          (lambda ()
            (interactive)
            (rust-enable-format-on-save)
            (column-marker-1 99)))

;; js2-mode
;; TODO(keunwoo): Find a better mode that doesn't require cl (deprecated)
;; (autoload 'js2-mode (format "js2" emacs-major-version) nil t)
;; (autoload 'js2-mode "js2" nil t)
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; from emacswiki:
;; After js2 has parsed a js file, we look for jslint globals decl
;; comment ("/* global Fred, _, Harry */") and add any symbols to a
;; buffer-local var of acceptable global vars Note that we also
;; support the "symbol: true" way of specifying names via a hack
;; (remove any ":true" to make it look like a plain decl, and any
;; ':false' are left behind so they'll effectively be ignored as you
;; can;t have a symbol called "someName:false"
(add-hook 'js2-post-parse-callbacks
          (lambda ()
            (when (> (buffer-size) 0)
              (let ((btext (replace-regexp-in-string
                            ": *true" " "
                            (replace-regexp-in-string
                             "[\n\t ]+"
                             " "
                             (buffer-substring-no-properties 1 (buffer-size))
                             t t))))
                (mapc (apply-partially 'add-to-list 'js2-additional-externs)
                      (split-string
                       (if (string-match "/\\* *global *\\(.*?\\) *\\*/" btext)
                           (match-string-no-properties 1 btext) "")
                       " *, *" t))))))

;; use fundamental for editing JSON (it's good enough, and js2 is too finicky)
;; (add-to-list 'auto-mode-alist '("\\.json$" . fundamental-mode))
;;
;; no, use json-mode
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
                        (setq js-indent-level 2)))

(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

;; MODE HOOKS TEMPLATE
;;
;; Sometimes these first two expressions are not necessary
;(assoc "\\.hpp$" auto-mode-alist)
;(setq auto-mode-alist (cons '("\\.hpp$" . c++-mode)
;                               auto-mode-alist))
;; Hook code:
;(add-hook 'html-mode-hook '(lambda () (column-number-mode 1)))

;; Text mode
(assoc "\\.txt$" auto-mode-alist)
(setq auto-mode-alist (cons '("\\.txt$" . paragraph-indent-text-mode)
                               auto-mode-alist))

;; Markdown mode
(assoc "\\.md$" auto-mode-alist)

;; Typescript
(require 'use-package)
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))
(add-hook 'typescript-mode-hook #'setup-tide-mode)
(use-package tide
  :ensure t
  :config
  (progn
    (company-mode +1)
    ;; aligns annotation to the right hand side
    (setq company-tooltip-align-annotations t)
    (add-hook 'typescript-mode-hook #'setup-tide-mode)
    ;; This is way too slow.
    ;; (add-hook 'before-save-hook 'tide-format-before-save) ; formats the buffer before saving
    (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
  ))
(defun my-tide-tsserver-locator ()
  "Locate the nearest relevant tsserver."
  (or
   ;; I don't know why tide doesn't do this by default.  Maybe I should send a PR.
   (let ((typescript-dir (concat
                          (locate-dominating-file default-directory ".git")
                          ;; (locate-dominating-file default-directory "node_modules/typescript")
                          "node_modules/typescript/lib/")))
     (tide--locate-tsserver typescript-dir))
   ;; Fall back to tide's default locator function.
   (tide-tscompiler-locater-npmlocal-projectile-npmglobal)))

;;; Use web-mode for html-like files.
(add-hook 'after-init-hook
          (lambda ()
            (require 'web-mode nil t)
            (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
            (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
            (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
            (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
            (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
            (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode)))
            (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode)))
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  ;; Enable tide in tsx files.
  (when (string-equal "tsx" (file-name-extension buffer-file-name))
    (setup-tide-mode)
    (column-enforce-mode))
  ;; Don't line up indents in various situations; standard indent is fine.
  (mapc
   (lambda (indent-situation)
     (add-to-list 'web-mode-indentation-params
                  (cons indent-situation nil)))
   '(
     "lineup-args"    ;; function arguments
     "lineup-calls"   ;; chained dotted calls
     "lineup-concats" ;; string concatenations
     "lineup-ternary" ;; ternary operator arguments
     )
   )
  ;; Delete trailing whitespace on save.
  (add-hook 'local-write-file-hooks
            (lambda ()
              (delete-trailing-whitespace)
              nil)))
(add-hook 'web-mode-hook 'my-web-mode-hook)

;; CSS mode
(add-hook 'css-mode-hook
          '(lambda ()
             (setq indent-tabs-mode 't)))

;; LaTeX mode
(add-hook 'latex-mode-hook
          '(lambda ()
             (interactive)
             (auto-fill-mode 0)
             (font-lock-mode)
             ;; Sometimes, for more elaborate LaTeX files, I use make
             ;; instead of the built-in Emacs latex compile command.
             (define-key tex-mode-map "\C-c\C-m" 'compile)))

;; Ruby mode
(add-hook 'ruby-mode-hook
          '(lambda () (interactive)
             (font-lock-mode)
             (setq-default ruby-indent-level 4)))

(add-hook 'python-mode-hook
          '(lambda() (interactive)
             ;; Fuck Python and your fucking "show me the Python
             ;; version" keybinding.  That is really something I need
             ;; on a motherfucking keybinding.  Not.
             (local-set-key "\C-c\C-v" 'compile)))

;; SML mode
;;;### (autoloads (sml-yacc-mode sml-lex-mode sml-cm-mode sml-mode)
;;;;;;  "sml-mode" "sml-mode.el" (14918 21945))
;;; Generated autoloads from sml-mode.el

(add-to-list (quote auto-mode-alist)
             (quote ("\\.s\\(ml\\|ig\\)\\'" . sml-mode)))

(autoload (quote sml-mode) "sml-mode" "\
\\<sml-mode-map>Major mode for editing ML code.
This mode runs `sml-mode-hook' just before exiting.
\\{sml-mode-map}" t nil)

(add-to-list (quote completion-ignored-extensions) "CM/")

(add-to-list (quote auto-mode-alist) (quote ("\\.cm\\'" . sml-cm-mode)))

(autoload (quote sml-cm-mode) "sml-mode" "\
Major mode for SML/NJ's Compilation Manager configuration files." t nil)

(autoload (quote sml-lex-mode) "sml-mode" "\
Major Mode for editing ML-Lex files." t nil)

(add-to-list (quote auto-mode-alist) (quote ("\\.grm\\'" . sml-yacc-mode)))

(autoload (quote sml-yacc-mode) "sml-mode" "\
Major Mode for editing ML-Yacc files." t nil)

;;;***

;;;### (autoloads nil "sml-proc" "sml-proc.el" (14918 21909))
;;; Generated autoloads from sml-proc.el

(autoload (quote run-sml) "sml-proc" nil t)

;;;***

;; Tuareg-mode (better than ocaml-mode)
(setq auto-mode-alist
  (cons '("\\.ml\\w?" . tuareg-mode) auto-mode-alist))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code." t)
(autoload 'camldebug "cameldeb" "Run the Caml debugger." t)

;; OCaml mode
;(setq auto-mode-alist
;      (cons '("\\.ml[iylp]?$" . caml-mode) auto-mode-alist))
;(autoload 'caml-mode "caml" "Major mode for editing Caml code." t)
;(autoload 'run-caml "inf-caml" "Run an inferior Caml process." t)

;; Cecil mode(s)
(if (load "cecil-mode.el" 't)
    (progn
      (assoc "\\.\\(cecil\\|diesel\\)$" auto-mode-alist)
      (setq auto-mode-alist (cons '("\\.\\(cecil\\|diesel\\)$" . cecil-mode)
                                  auto-mode-alist))
      (add-hook 'cecil-mode-hook '(lambda () (font-lock-mode)))

      ;; Experimental WIL mode
      ;;(load "wil-mode.el")
      ;;(assoc "\\.wil$" auto-mode-alist
      ;;(setq auto-mode-alist (cons '("\\.wil$" . wil-mode)
      ;;                               auto-mode-alist))
      ;;(add-hook 'wil-mode-hook '(lambda () (font-lock-mode)))
      ))

;; MultiJava/HydroJ uses JDE mode
(setq auto-mode-alist (cons '("\\.\\(mj\\|hj\\)$" . jde-mode)
                            auto-mode-alist))
;;(add-hook 'jde-mode-hook
;;          '(lambda ()
;;             (define-key jde-mode-map "\C-c\C-v\C-c" 'compile)))

;; compilation mode
(require 'compile)
;; Recipe from https://stackoverflow.com/questions/13397737/ansi-coloring-in-compilation-mode
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; C/C++ modes.
(add-hook 'c++-mode-hook
          '(lambda ()
             (setq ps-print-color-p 't)))
;; TODO(keunwoo): implement the new way to do this in Emacs 26.3 and up...
;; From https://stackoverflow.com/questions/23553881/emacs-indenting-of-c11-lambda-functions-cc-mode
;; It is embarrassing that this is necessary.
;; Update to emacs 27: maybe this isn't necessary anymore?
;; Need to edit some C++ code and see.
;; (defadvice c-lineup-arglist (around my activate)
;;   "Improve indentation of continued C++11 lambda function opened as argument."
;;   (setq ad-return-value
;;         (if (and (equal major-mode 'c++-mode)
;;                  (ignore-errors
;;                    (save-excursion
;;                      (goto-char (c-langelem-pos langelem))
;;                      ;; Detect "[...](" or "[...]{". preceded by "," or "(",
;;                      ;;   and with unclosed brace.
;;                      (looking-at ".*[(,][ \t]*\\[[^]]*\\][ \t]*[({][^}]*$"))))
;;             0                           ; no additional indent
;;           ad-do-it)))                   ; default behavior

; I haven't found a good editing mode for arbitrary XML.  sgml-mode
; appears to require DTDs or something.  I don't want or need a
; validating parser, just indents and proper line wrap!
;(assoc "\\.xml" auto-mode-alist)
;(setq auto-mode-alist (cons '("\\.xml" . sgml-mode) auto-mode-alist))

;; external scheme mode
(setq scheme-program-name "/scratch/usr/bin/scheme")
(setq scheme-program-arguments "-library /scratch/usr/lib/mit-scheme")

;; LaTeX mode (some Emacs versions seem to use SliTeX instead; annoying)
(assoc "\\.tex$" auto-mode-alist)
(setq auto-mode-alist (cons '("\\.tex$" . latex-mode)
                               auto-mode-alist))
(add-hook 'latex-mode-hook
          '(lambda ()
             (font-lock-mode)

             ;; enables inverse search in KDVI
             ;; currently broken, don't know why
             ;; (gnuserv-start)

             ))
;; DVI viewer for LaTeX mode.
(setq tex-dvi-view-command
      (if (eq window-system 'x)
          "evince"
          "dvi2tty * | cat -s"))

; Sometimes, for more elaborate LaTeX files, I use make instead of
; the built-in Emacs latex compile command.
(add-hook 'tex-mode-hook
          '(lambda ()
             (define-key tex-mode-map "\C-c\C-m" 'compile)))

(assoc "\\.cyc$" auto-mode-alist)
(setq auto-mode-alist (cons '("\\.cyc$" . c++-mode)
                            auto-mode-alist))

;; Decorativeness of various modes
; (setq-default font-lock-maximum-decoration
;              '((cecil-mode . 2) (c++-mode . 3) (t . 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UTILITIES

(defun hex-to-formatted-time (usec-hex-string)
  (format-time-string
   "%Y-%m-%d %H:%M:%S"
   (seconds-to-time (/ (string-to-number hex-string) 1000000.0))))

;;; Extracts path extension from PATH=...:$PATH lines in shell scripts.
;;; TODO(keunwoo): dedupe entries in paths.
;;; TODO(keunwoo): make this work on Windows
;;; TODO(keunwoo): add another function that extracts paths from /etc/paths{,.d}
(defun path-extensions-from-shell-script (shell-script-path)
    (if (eq system-type 'windows-nt)
        nil
      (let*
          ((strip
            ;; Strips down a .profile line of the following form:
            ;;   export PATH=foo:bar:baz  # optional comment
            ;; or
            ;;   PATH=foo:bar:baz  # another comment
            ;; removing leading/trailing whitespace, and the leading
            ;; "export" and "PATH=", to produce just
            ;;   foo:bar:baz
            (lambda (s)
              (let* (
                     ;; strip comments
                     (s (replace-regexp-in-string "#.*$" "" s 't 't))
                     ;; strip PATH=
                     (s (replace-regexp-in-string
                         "^[ \t\n]*\\(export \\)?PATH=" "" s 't 't))
                     ;; strip leading & trailing quote & whitespace
                     (s (replace-regexp-in-string
                         "^[ \t\n]*\"?\\([^ \t\n\"]*\\)\"?[ \t\n]*$" "\\1" s 't nil)))
                s)))

           ;; Extract "PATH=" or "export PATH=" lines from file.
           (path-extensions
            (let ((temp-buffer (generate-new-buffer "*path-grep*")))
              (unwind-protect
                  (progn
                    (call-process "grep" nil temp-buffer nil
                                  "-E"
                                  "^[[:space:]]*(export )?PATH="
                                  shell-script-path)
                    (with-current-buffer temp-buffer
                      (mapcar (lambda (s) (funcall strip s))
                              (delete "" (split-string (buffer-string) "\n")))))
                (kill-buffer temp-buffer))))

           ;; Expand $HOME/foo/bar, replacing HOME with its value.
           (expanded-path-extensions
            (let ((home (getenv "HOME")))
              (mapcar (lambda (line)
                        (replace-regexp-in-string "[$]{?HOME}?" home line 't 't))
                      path-extensions))))

        expanded-path-extensions)))

;; Given path extension expressions extracted by path-extensions-from-shell-script,
;; returns the final path resulting from extending the current path.
(defun extend-executable-path (path-extensions)
    (let* ((current-path (getenv "PATH")))
      (progn
        ;; Substitutes PATH sequentially at the appropriate part in each
        ;; subsequent declaration.
        (dolist (extension path-extensions)
          (setq current-path
                (replace-regexp-in-string "[$]{?PATH}?" current-path extension
                                          't 't)))
        current-path)))

;; Update PATH based on the path extension expressions parsed from paths,
;; which is a list of homedir-relative paths.
(defun extend-executable-path-from-homedir-paths (paths)
  (let* ((home (getenv "HOME"))
         (path-extensions-from-homedir
          (lambda (file-name)
            (let ((file-path (concat home "/" file-name)))
              (if (file-exists-p file-path)
                  (path-extensions-from-shell-script file-path)
                nil))))
         (all-path-extensions (mapcar path-extensions-from-homedir paths))
         (flattened-path-extensions (apply 'append all-path-extensions))
         (updated-path (extend-executable-path flattened-path-extensions)))
    (progn
      (setenv "PATH" updated-path)
      (set-variable 'exec-path (split-string (getenv "PATH") ":"))
      updated-path)))

(extend-executable-path-from-homedir-paths '(".profile" ".cargo/env"))

;; Try valiantly to make Windows a semi-acceptable dev environment.
(if (eq system-type 'windows-nt)
    (progn
      (setenv "PATH"
              (mapconcat (lambda (v) v)
                         (append (split-string (getenv "PATH") ";")
                                 '(
                                   "c:\\Program Files\\Git\\bin"
                                   "c:\\Program Files\\Git\\usr\\bin"
                                   ))
                         ";"))
      (set-variable 'exec-path (split-string (getenv "PATH") ";")))
  )


;; I always write ~/lib/emacs/site-lisp-keunwoo.el that provides my
;; site-specific customizations, as follows:
;;
;; (provide 'site-lisp-keunwoo)
;;
;; The following line loads the above lib.
(require 'site-lisp-keunwoo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOMIZE

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(c-offsets-alist '((innamespace . 0)))
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
 '(prettier-js-command "/Users/keunwoo/bin/run-prettier")
 '(ps-print-header-frame nil)
 '(safe-local-variable-values
   '((eval rename-buffer "*notes*")
     (buffer-file-coding-system . utf-8-dos)
     (css-indent-offset . 2)))
 '(scroll-bar-mode 'right)
 '(show-trailing-whitespace t)
 '(tide-node-executable "/Users/keunwoo/bin/node-activated")
 '(tide-sync-request-timeout 5)
 '(tide-tsserver-locator-function 'my-tide-tsserver-locator)
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

(when window-system
  (cond ((eq window-system 'ns)
         (progn
           ;; I was trying out Inconsolata for a while, and I might
           ;; give it another shot someday.  It's very nice, but it
           ;; doesn't have a bold weight.  I was fairly surprised that
           ;; this bothered me, but it did.
           ;; ((x-family-fonts "Inconsolata")
           ;;  (custom-set-faces
           ;;   '(default ((t (:family "Inconsolata" :height 130))))))

           ;; ...so Menlo it is on OSX.
           ;; Menlo is just a tweaked version of DejaVu Sans Mono.
           (custom-set-faces
            '(default ((t (:family "Menlo" :height 110)))))))

        ((eq window-system 'w32)
         (custom-set-faces
          '(default ((t (:inherit nil :stipple nil :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "outline" :family "Consolas"))))))

        (t
         (custom-set-faces
          ;; Currently this is ok for ChromeOS/Crostini.
          ;; If I ever go back to a proper Linux desktop, we'll have to edit this.
          '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 80 :width normal :foundry "GOOG" :family "Noto Mono"))))))))

;; A color scheme that's less obtrusive than the Emacs default.
;; Guarded with window-system because many terminals render subtle colors badly.
(if window-system
  (custom-set-faces
   '(column-enforce-face ((t (:inherit nil :underline "#966"))))
   '(flycheck-error ((t (:underline "orchid3"))))
   '(flycheck-info ((t (:underline "ForestGreen"))))
   '(font-lock-comment-face ((t (:foreground "#997777"))))
   '(font-lock-function-name-face ((t (:foreground "#0226cc"))))
   '(font-lock-keyword-face ((t (:foreground "#8a0f00"))))
   '(font-lock-string-face ((t (:foreground "#338300"))))
   '(font-lock-type-face ((t (:foreground "#665500"))))
   '(font-lock-variable-name-face ((t (:foreground "#4a708b"))))
   '(helm-ls-git-modified-not-staged-face ((t (:foreground "yellow4"))))
   '(mode-line ((t (:background "#e5e5e5" :box nil))))
   '(trailing-whitespace ((t (:background "gray95"))))
   )
  ;; In terminal, just use less boldface and yellow.
  (custom-set-faces
   '(font-lock-comment-face ((t nil)))
   '(font-lock-function-name-face ((t (:foreground "blue"))))
   '(trailing-whitespace ((t (:background "white"))))
   '(web-mode-html-attr-name-face ((t nil)))
   '(web-mode-html-tag-bracket-face ((t nil)))
   '(web-mode-html-tag-face ((t nil)))
   '(whitespace-indentation ((t (:foreground "firebrick"))))
   '(whitespace-tab ((t (:foreground "yellow")))))
  )

(put 'scroll-left 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
