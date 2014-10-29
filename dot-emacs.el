;; -*- emacs-lisp -*-
;; Keunwoo Lee's .emacs file.
;; Updated for GNU Emacs 24.3

;;;;;;;;;;;;;;;;;;;;;; PRELIMINARIES ;;;;;;;;;;;;;;;;;;;;

;; Custom load paths
(add-to-list 'load-path (concat (getenv "HOME") "/lib/emacs"))
(add-to-list 'load-path (concat (getenv "HOME") "/lib/site-emacs"))

;;;;;;;;;;;;;;;;;;;;;;; EDITING ;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;; DISPLAY ;;;;;;;;;;;;;;;;;;;;;;;

;; That splash screen is idiotic.
(setq inhibit-splash-screen t)

;; Turn font lock on for all modes
(if (string-match "XEmacs" emacs-version)
    ;; XEmacs
    (font-lock-mode)
  ;; FSF
  (global-font-lock-mode 't))

;; Who needs a 3D modeline?
(if (string-match "XEmacs" emacs-version)
    (set-specifier modeline-shadow-thickness 0))

;; Or a toolbar?
(if (string-match "XEmacs" emacs-version)
    (set-specifier default-toolbar-visible-p nil)
  ;; FSF only has a toggle, not a "turn it off" function.
  (tool-bar-mode -1))

; Or a menubar?
(if (string-match "XEmacs" emacs-version)
    ;; XEmacs.  To re-enable: M-x set-specifier menubar-visible-p 't
    (set-specifier menubar-visible-p nil)
  ;; FSF
  (menu-bar-mode nil))

;; Or a #@&$!&@ blinking cursor?
(if (not (string-match "XEmacs" emacs-version))
    (blink-cursor-mode nil))

;; I know what I'm doing w.r.t. key mappings; don't warn me.
(if (string-match "XEmacs" emacs-version)
    (setq display-warning-suppressed-classes
          (cons 'key-mapping display-warning-suppressed-classes)))

;; Start gnuserv (enables inverse search in KDVI)
;;(gnuserv-start)

; Save faces with all other options
(setq options-save-faces t)

; Underlining sucks.
(setq face-underline-p nil)

; Always show line and col no.
(line-number-mode 1)
(column-number-mode 1)

;(require 'column-marker)
(add-hook 'java-mode-hook
          '(lambda ()
             (font-lock-set-up-width-warning 100)
             (c-set-offset 'arglist-intro '++)
             (c-set-offset 'arglist-cont 0)
             (c-set-offset 'arglist-cont-nonempty '++)))

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

; Abbreviation expansion.  Not useful for me, because I use languages
; that actually make use of the backquote.
; (global-set-key "`" 'dabbrev-expand)

; I don't know why one would set this any other way, since C-d does
; forward delete... whatever.
(setq delete-key-deletes-forward nil)

; Convenient compilation macro; this key combo is used on very few
; modes, and happens to be the same keystroke used to view TeX output
(define-key global-map "\C-c\C-v" 'compile)
(define-key global-map "\C-c\C-m" 'compile)

;; find-file provides functions to switch between spec and body
;; (useful in Ada and C++, which languages define specifications and
;; bodies of packages in separate files).
;;
;; Q: Why don't those $%&!ing Emacs people define a function
;; "defined-p" so users don't have to match on the Emacs version
;; string?
(if (string-match "XEmacs" emacs-version)
    (let ((find-file-package-installed-p (file-installed-p "find-file.el")))
      (if find-file-package-installed-p
          (progn
            (require 'find-file "find-file")
            (setq-default cc-other-file-alist
                          '(
                            ("\\.cc$"  (".hh" ".h"))
                            ("\\.hh$"  (".cc" ".C"))

                            ("\\.c$"   (".h"))
                            ("\\.h$"   (".c" ".cc" ".C" ".CC" ".cxx" ".cpp"))

                            ("\\.C$"   (".H"  ".hh" ".h"))
                            ("\\.H$"   (".C"  ".CC"))

                            ("\\.CC$"  (".HH" ".H"  ".hh" ".h"))
                            ("\\.HH$"  (".CC"))

                            ("\\.cxx$" (".hh" ".h"))
                            ("\\.cpp$" (".hpp" ".hh" ".h"))
                            ("\\.hpp$" (".cpp"))
                            ))
            (define-key global-map "\C-co" 'ff-find-other-file)))))

;; Minibuffer hacks for FSF Emacs
(if (not (string-match "XEmacs" emacs-version))
    (progn

      ;; Tab-completion in minibuffer (thanks to Ami Fischman)
      (if (< 24 emacs-major-version)
          (defadvice read-from-minibuffer
            (around tab-is-pcomplete-in-minibuffer activate)
            "Bind TAB to pcomplete in minibuffer reads."
            (let ((keymap minibuffer-local-map))
              (define-key keymap "\t" 'pcomplete)
              (ad-set-arg 2 keymap)
              ad-do-it)))

      ;; Enable recursive minibuffers
      (set-variable 'enable-recursive-minibuffers 't)

      ))

; man page lookup (by default, f1 is help, but I already know how to
; bring that up using C-h)
(define-key global-map [f1]
  (lambda () (interactive) (manual-entry (current-word))))

; F2 to spawn another frame
(define-key global-map [f2] (lambda () (interactive) (make-frame)))

; F3 to kill the other window
(define-key global-map [f3] (lambda () (interactive) (delete-other-windows)))

; F4 for dired buffer of the current directory in the other window
(define-key global-map [f4]
  (lambda () (interactive) (dired-other-window default-directory)))

; F5 to quickly call *scratch* in the other window
(define-key global-map [f5]
  (lambda () (interactive) (switch-to-buffer-other-window "*scratch*")))

; F6 to rename the current buffer
(define-key global-map [f6]
  (lambda () (interactive)
    (rename-buffer
     (read-from-minibuffer
      "Rename current buffer: " ; Prompt in minibuffer
      (buffer-name)             ; Use current name initially
      ))))

; F7 to rotate custom colors
(defvar background-color-rotation
  '("white" "aliceblue" "thistle1" "lemonchiffon" "khaki" "papayawhip"
    "honeydew" "mistyrose" "paleturquoise" "transparent")
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
(define-key global-map [f7]
  (lambda () (interactive) (rotate-background-color)))

; F8 to open dired buffer of the current directory, without dotfiles
(define-key global-map [f8]
  (lambda () (interactive)
    (dired-other-window (concat default-directory "[^.]*"))))

; F9 to re-execute last compilation
(define-key global-map [f9]
  (lambda () (interactive) (recompile)))

; ibuffer is better than the normal buffer list.
(define-key global-map "\C-x\C-b" 'ibuffer)

; PrntScrn to invoke ps-print and generate an output PostScript file.
;(require 'ps-print)
;(define-key global-map [print]
;  (lambda ()
;    (interactive)
;    (setq ps-print-color-p t)
;    (ps-print-buffer-with-faces
;     (read-string "PS output filename: "))))

; Handy incremental scrolling keys
(define-key global-map "\M-n" (lambda () (interactive) (scroll-up 1)))
(define-key global-map "\M-p" (lambda () (interactive) (scroll-down 1)))

;; delete prior word on XEmacs (FSF Emacs has this by default)
(if (string-match "XEmacs" emacs-version)
    (define-key global-map '(control backspace) 'backward-kill-word))

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

;; js2-mode
;(autoload 'js2-mode (format "js2" emacs-major-version) nil t)
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
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
(add-to-list 'auto-mode-alist '("\\.json$" . fundamental-mode))

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

; Use XML/SGML-mode for .html files, and do not auto-fill
;; (assoc "\\.html$" auto-mode-alist)
;; (if (string-match "XEmacs" emacs-version)
;;     (progn
;;       (setq auto-mode-alist
;;             (cons '("\\.html$" . xml-mode) auto-mode-alist))
;;       (add-hook 'xml-mode-hook
;;                 '(lambda ()
;;                    (auto-fill-mode nil)
;;                    ;; this is useful for editing Apache Ant files
;;                    (define-key xml-mode-map "\C-c\C-v" 'compile))
;;                 '(lambda ()
;;                    (auto-fill-mode nil)
;;                    (define-key xml-mode-map "\C-c\C-t"
;;                      '(lambda () (interactive)
;;                         (html-helper-default-insert-timestamp))))))
;;   ;; FSF
;;   (progn
;;     (setq auto-mode-alist
;;           (cons '("\\.html$" . sgml-mode) auto-mode-alist))
;;     (auto-fill-mode nil)))

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
;(add-hook 'jde-mode-hook
;          '(lambda ()
;             (define-key jde-mode-map "\C-c\C-v\C-c" 'compile)))
(require 'compile)
(if (string-match "XEmacs" emacs-version)
    (progn
      (set-variable
       'compilation-error-regexp-alist-alist
       (cons
        '(all
          ("\\File \"\\(.*\\)\", line \\([0-9]+\\), character \\([0-9]+\\).*:"
           1 2 3))
        compilation-error-regexp-alist-alist))
      (compilation-build-compilation-error-regexp-alist)))

;; C/C++ modes.
(add-hook 'c-mode-hook
          '(lambda ()
             (font-lock-mode)))
(add-hook 'c++-mode-hook
          '(lambda ()
             (font-lock-mode)
             (setq ps-print-color-p 't)))

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
;; DVI viewer for LaTeX mode (I think XEmacs wants to Customize this
;; itself, but whatever)
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

;; Set PATH as specified in ~/.profile.
;; TODO(keunwoo): dedupe entries in paths.
;; TODO(keunwoo): make this work on Windows
(if (not (eq system-type 'windows-nt))
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
            (let* ((s (replace-regexp-in-string "#.*$" "" s 't 't))
                   (s (replace-regexp-in-string
                       "^\\(export \\)?PATH=" "" s 't 't))
                   (s (replace-regexp-in-string
                       "^[ \t\n]*\\([^ \t\n].*\\)$" "\\1" s 't nil))
                   (s (replace-regexp-in-string
                       "^\\(.*[^ \t\n]\\)[ \t\n]*$" "\\1" s 't nil)))
              s)))

         ;; Extract "PATH=" or "export PATH=" lines from .profile.
         (profile-paths
          (let ((temp-buffer (generate-new-buffer "*path-grep*")))
            (unwind-protect
                (progn
                  (call-process "grep" nil temp-buffer nil
                                "-E"
                                "^(export )?PATH="
                                (concat (getenv "HOME") "/.profile"))
                  (with-current-buffer temp-buffer
                    (mapcar (lambda (s) (funcall strip s))
                            (delete "" (split-string (buffer-string) "\n")))))
              (kill-buffer temp-buffer))))

         ;; Expand $HOME/foo/bar, replacing HOME with its value.
         (profile-paths-expanded
          (let ((home (getenv "HOME")))
            (mapcar (lambda (line)
                      (replace-regexp-in-string "[$]{?HOME}?" home line 't 't))
                    profile-paths)))

         (current-path (getenv "PATH")))

      ;; Substitutes PATH sequentially at the appropriate part in each
      ;; subsequent declaration.
      (dolist (profile-path profile-paths-expanded)
        (setq current-path
              (replace-regexp-in-string "[$]{?PATH}?" current-path profile-path
                                        't 't)))

      ;; Return final, concatenated/expanded path.
      (setenv "PATH" current-path)))

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
 '(column-number-mode t)
 '(elisp-cache-byte-compile-files t)
 '(ibuffer-enable t)
 '(ibuffer-formats (quote ((mark modified read-only " " (name 32 32 :left :elide) " " (size 9 -1 :right) " " (mode 16 16 :left :elide) " " filename-and-process) (mark " " (name 16 -1) " " filename))))
 '(longlines-show-hard-newlines nil)
 '(longlines-wrap-follows-window-size t)
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("melpa" . "http://melpa.milkbox.net/packages/"))))
 '(ps-print-header-frame nil)
 '(safe-local-variable-values (quote ((css-indent-offset . 2))))
 '(scroll-bar-mode (quote right))
 '(tool-bar-mode nil)
 '(vc-follow-symlinks nil)
 '(visible-bell t)
 '(visible-cursor nil))

(when (and window-system (not (eq window-system 'ns)))
  (custom-set-faces
   '(default ((t (:inherit nil :stipple nil :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 83 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
   '(font-lock-comment-face ((nil (:foreground "goldenrod4"))))
   '(font-lock-function-name-face ((nil (:foreground "blue"))))
   '(font-lock-keyword-face ((t (:foreground "maroon"))))
   '(font-lock-string-face ((nil (:foreground "forestgreen"))))
   '(font-lock-type-face ((t (:foreground "darkorange3"))))
   '(font-lock-variable-name-face ((((class color) (background light)) (:foreground "blue4"))))
   '(mode-line ((t (:background "grey90" :foreground "black" :box nil))))
   '(trailing-whitespace ((((class color) (background light)) (:background "gray90")))))
  )

(when (and window-system (eq window-system 'ns))
  (custom-set-faces
   '(default ((t (:height 120 :family "Menlo")))))
  )
