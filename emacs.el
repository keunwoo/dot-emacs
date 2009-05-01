;; -*- emacs-lisp -*-
;; Keunwoo Lee's .emacs file.  I use the same .emacs file regularly on
;; Linux FSF Emacs 21.2.1, XEmacs 21.4, and Solaris 8 FSF Emacs 21.1.1.

;;;;;;;;;;;;;;;;;;;;;; PRELIMINARIES ;;;;;;;;;;;;;;;;;;;;

;; Custom load paths

;; On my personal machines
(add-to-list 'load-path "/home/klee/cecil/vortex/Cecil/src/emacs")
(if (string-match "XEmacs" emacs-version)
    ; XEmacs
    (add-to-list 'load-path "/home/klee/lib/xemacs")
  ; FSF Emacs
  (add-to-list 'load-path "/home/klee/lib/emacs"))

;; On my desktop at UW
(add-to-list 'load-path "/homes/gws/klee/lib/emacs") ; 

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

;;;;;;;;;;;;;;;;;;;;;;; DISPLAY ;;;;;;;;;;;;;;;;;;;;;;;

(if (string-match "XEmacs" emacs-version)
    (progn ; XEmacs stuff
      ;; XEmacs way of turning font lock for all modes
      (font-lock-mode)

      ;; Who needs a 3D modeline?
      (set-specifier modeline-shadow-thickness 0)

      ;; Or a toolbar?
      (set-specifier default-toolbar-visible-p nil)

      ;; I know what I'm doing w.r.t. key mappings; don't warn me.
      (setq display-warning-suppressed-classes
            (cons 'key-mapping display-warning-suppressed-classes))

      ;; Start gnuserv (enables inverse search in KDVI)
      ;;(gnuserv-start)

      )
      
  (progn ; FSF Emacs stuff
    ;; FSF Emacs's way of turning on font lock for all modes.
    (global-font-lock-mode 't)

    ;; Turn off FSF Emacs toolbar
    (if (boundp 'tool-bar-mode) (tool-bar-mode nil))

    ;; Blinking cursor is spawn of the Evil One
    (blink-cursor-mode nil)

    ;; Set default font & other window properties under FSF Emacs.
    ;; XEmacs doesn't need this because it uses the X resources
    ;; database, which can be modified in ~/.Xdefaults
    (setq default-frame-alist
          (append default-frame-alist
                  '((width . 80) (height . 50))))
    ; (set-default-font "-*-courier-medium-r-normal-*-*-120-*-*-*-*-*-*")
    ; (set-background-color "aliceblue")

    ;; Startup editor server.
    (server-start)
    ))

; Save faces with all other options
(setq options-save-faces t)

; Underlining sucks.
(setq face-underline-p nil)

; Always show line and col no.
(line-number-mode 1) 
(column-number-mode 1)

; Get rid of the damn menu.  If I want it back, I can always invoke
; M-x set-specifier menubar-visible-p 't
(if (string-match "XEmacs" emacs-version)
    (set-specifier menubar-visible-p nil)
  ;; FSF way of doing the same; to undo, do M-x menu-bar-mode
  (menu-bar-mode nil))


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

; F8 to open dired buffer of the current directory, without dotfiles
(define-key global-map [f8]
  (lambda () (interactive) (dired-other-window (concat default-directory "[^.]*"))))

; F9 to kill buffer
(define-key global-map [f9]
  (lambda () (interactive) (kill-buffer (current-buffer))))

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

; Mouse wheel: scroll up, scroll down; hold down control to do pgup/pgdn.
(defun wheel-scroll-up   ()   (lambda () (interactive) (scroll-up 2)))
(defun wheel-scroll-down ()   (lambda () (interactive) (scroll-down 2)))
(defun wheel-scroll-pgup ()   (lambda () (interactive) (scroll-up 20)))
(defun wheel-scroll-pgdown () (lambda () (interactive) (scroll-down 20)))
(cond
 ((string-match "XEmacs" emacs-version)
  (progn
    (define-key global-map 'button5 (wheel-scroll-up))
    (define-key global-map 'button4 (wheel-scroll-down))
    (define-key global-map '(control button5) (wheel-scroll-pgup))
    (define-key global-map '(control button4) (wheel-scroll-pgdown))))
 (t ; FSF Emacs uses weird [bracket] keymap specifiers.
  (progn
    (define-key global-map [mouse-5] (wheel-scroll-up))
    (define-key global-map [mouse-4] (wheel-scroll-down))
    (define-key global-map [C-mouse-5] (wheel-scroll-pgup))
    (define-key global-map [C-mouse-4] (wheel-scroll-pgdown)))))

;; Make alt behave like Meta for all the commands I use frequently.
;; Handy when using a single instance of Emacs simultaneously on
;; multiple X servers that disagree on how to treat alt/meta.  And,
;; actually, this has the side benefit that if I accidentally hit
;; alt-k for some k that I never use, it's probably an accident, and
;; nothing will happen.
(cond
 ((string-match "XEmacs" emacs-version)
  (progn
    (define-key global-map '(alt a) 'backward-sentence)
    (define-key global-map '(alt b) 'backward-word)
    (define-key global-map '(alt c) 'capitalize-region-or-word)
    (define-key global-map '(alt d) 'kill-word)
    (define-key global-map '(alt e) 'forward-sentence)
    (define-key global-map '(alt f) 'forward-word)
    (define-key global-map '(alt l) 'downcase-region-or-word)
    (define-key global-map '(alt n) (lambda () (interactive) (scroll-up 1)))
    (define-key global-map '(alt p) (lambda () (interactive) (scroll-down 1)))
    (define-key global-map '(alt q) 'fill-paragraph-or-region)
    (define-key global-map '(alt t) 'transpose-words)
    (define-key global-map '(alt u) 'upcase-region-or-word)
    (define-key global-map '(alt v) 'scroll-down-command)
    (define-key global-map '(alt w) 'kill-ring-save)
    (define-key global-map '(alt y) 'yank-pop)
    (define-key global-map '(alt x) 'execute-extended-command)
    (define-key global-map '(alt y) 'yank-pop)
    (define-key global-map '(alt !) 'shell-command)
    (define-key global-map '(alt <) 'beginning-of-buffer)
    (define-key global-map '(alt >) 'end-of-buffer)
    (define-key global-map '(alt {) 'backward-paragraph)
    (define-key global-map '(alt }) 'forward-paragraph)
    (define-key global-map '(alt ~) 'not-modified)))
 (t
  (progn
    (define-key global-map [?\A-a] 'backward-sentence)
    (define-key global-map [?\A-b] 'backward-word)
    (define-key global-map [?\A-c] 'capitalize-word)
    (define-key global-map [?\A-d] 'kill-word)
    (define-key global-map [?\A-e] 'forward-sentence)
    (define-key global-map [?\A-f] 'forward-word)
    (define-key global-map [?\A-l] 'downcase-word)
    (define-key global-map [?\A-n] (lambda () (interactive) (scroll-up 1)))
    (define-key global-map [?\A-p] (lambda () (interactive) (scroll-down 1)))
    (define-key global-map [?\A-q] 'fill-paragraph)
    (define-key global-map [?\A-t] 'transpose-words)
    (define-key global-map [?\A-u] 'upcase-word)
    (define-key global-map [?\A-v] 'scroll-down)
    (define-key global-map [?\A-w] 'kill-ring-save)
    (define-key global-map [?\A-y] 'yank-pop)
    (define-key global-map [?\A-x] 'execute-extended-command)
    (define-key global-map [?\A-!] 'shell-command)
    (define-key global-map [?\A-<] 'beginning-of-buffer)
    (define-key global-map [?\A->] 'end-of-buffer)
    (define-key global-map [?\A-{] 'backward-paragraph)
    (define-key global-map [?\A-}] 'forward-paragraph)
    (define-key global-map [?\A-~] 'not-modified))))


;;;;;;;;;;;;;;;;;;;;;;; MODE HOOKS ETC ;;;;;;;;;;;;;;;;;;;;;;;

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
(add-hook 'text-mode-hook
        '(lambda ()
           (progn
             ; Auto fill in all text-based modes (text, parindent, etc.)
             (auto-fill-mode 1)

             ; For Parindent mode, don't give me that irritating
             ; behavior that tries to "adaptive fill" on ordinary
             ; indented paragraphs.
             (if (string-equal mode-name "Parindent")
                 (progn
                   (make-variable-buffer-local 'adaptive-fill-regexp)
                   (setq adaptive-fill-regexp nil))))))

(define-key text-mode-map "\t"
  '(lambda ()
     (interactive)
     ; In Parindent mode, do a simple tab.  Otherwise,
     ; use the fancy tabbing mode.
     (if (string-equal mode-name "Parindent")
       (tab-to-tab-stop)
       (indent-relative))
     ))

; Use XML/SGML-mode for .html files, and do not auto-fill
(assoc "\\.html$" auto-mode-alist)
(if (string-match "XEmacs" emacs-version)
    ;; XEmacs way
    (progn
      (setq auto-mode-alist
            (cons '("\\.html$" . xml-mode) auto-mode-alist))
      (add-hook 'xml-mode-hook
                '(lambda ()
                   (auto-fill-mode nil)
                   ;; this is useful for editing Apache Ant files
                   (define-key xml-mode-map "\C-c\C-v" 'compile)))

                '(lambda ()
                   (auto-fill-mode nil)
                   (define-key xml-mode-map "\C-c\C-t"
                     '(lambda () (interactive) (html-helper-default-insert-timestamp)))))
  ;; FSF way
  (progn
    (add-hook 'sgml-mode-hook
              '(lambda () (auto-fill-mode nil)))))

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

;; SML mode
;;;### (autoloads (sml-yacc-mode sml-lex-mode sml-cm-mode sml-mode)
;;;;;;  "sml-mode" "sml-mode.el" (14918 21945))
;;; Generated autoloads from sml-mode.el

(add-to-list (quote auto-mode-alist) (quote ("\\.s\\(ml\\|ig\\)\\'" . sml-mode)))

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

;; C/C++ modes.  Q: why did I setq-default basic offset here when I
;; also do it above?
(add-hook 'c-mode-hook
          '(lambda ()
             (font-lock-mode)
             (setq-default c-basic-offset 4)))
(add-hook 'c++-mode-hook
          '(lambda ()
             (font-lock-mode)
             (setq ps-print-color-p 't)
             (setq c-basic-offset 4)))

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
          "kdvi"
          ;"xdvi"
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following are "customize" settings.  Customize settings are
;; normally set automatically, but I find that the default behaviors
;; don't play nice when you have to use multiple Emacs versions and
;; flavors.  So, I hand-hack them.

(if (string-match "XEmacs" emacs-version)
    (progn
      ;; XEmacs 21.4 only
      (if (string-match "21.4" emacs-version) 
          (progn  
            (set-specifier top-gutter-visible-p nil)
            (custom-set-variables
             '(progress-feedback-use-echo-area 't)
             '(load-home-init-file t t)
             '(zmacs-regions nil)
             )))

      ;; All XEmacs versions
      (setq inhibit-startup-message t)
      (custom-set-variables
       '(load-home-init-file t t)
       ))
  (progn
    ;; FSF Emacs settings
    ;; nothing for now
    ))

;; Stuff that has to be set after customize runs, in order to not be
;; heinously annoying.
(if (string-match "XEmacs" emacs-version)
    (progn
      (require 'psgml-html)
      (setq write-file-hooks '(html-helper-update-timestamp))))

;; Generic Emacs (XEmacs or FSF)
(put 'narrow-to-region 'disabled nil)
(setq minibuffer-max-depth nil)

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "aliceblue" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 116 :width normal :family "schumacher-clean"))))
 '(bold ((t (:foreground "darkred" :size "12"))))
 '(bold-italic ((t (:italic nil))))
 '(dired-face-directory ((((type x pm mswindows tty) (class color)) (:foreground "blue3"))))
 '(dired-face-symlink ((((class color)) (:foreground "saddlebrown"))))
 '(font-latex-sectioning-1-face ((t (:inherit font-latex-sectioning-2-face :height 1.0))))
 '(font-latex-sectioning-2-face ((t (:inherit font-latex-sectioning-3-face))))
 '(font-latex-sectioning-3-face ((t (:inherit font-latex-sectioning-4-face))))
 '(font-lock-builtin-face ((((class color) (background light)) (:foreground "plum4"))))
 '(font-lock-comment-face ((((class color) (background light)) (:foreground "goldenrod4"))))
 '(font-lock-constant-face ((((class color) (background light)) (:foreground "green4"))))
 '(font-lock-function-name-face ((((class color) (background light)) (:foreground "brown4"))))
 '(font-lock-interface-def-face ((t (:foreground "blue"))))
 '(font-lock-keyword-face ((((class color) (background light)) (:foreground "darkmagenta"))))
 '(font-lock-module-def-face ((t (:foreground "red4"))))
 '(font-lock-reference-face ((((class color) (background light)) (:foreground "purple4"))))
 '(font-lock-string-face ((t (:foreground "purple"))))
 '(font-lock-type-def-face ((t (:foreground "blue4"))))
 '(font-lock-type-face ((t (:foreground "royalblue"))))
 '(font-lock-variable-name-face ((((class color) (background light)) (:foreground "blue4"))))
 '(gnus-summary-low-ancient-face ((((class color) (background light)) (:foreground "RoyalBlue"))))
 '(gnus-summary-low-read-face ((((class color) (background light)) (:foreground "DarkGreen"))))
 '(gnus-summary-low-ticked-face ((((class color) (background light)) (:foreground "firebrick"))))
 '(gnus-summary-low-unread-face ((t nil)))
 '(html-helper-bold-face ((t (:foreground "darkred"))))
 '(html-helper-italic-face ((t (:foreground "red4"))))
 '(hyper-apropos-section-heading ((t (:foreground "royalblue"))))
 '(info-node ((t (:foreground "maroon"))))
 '(info-xref ((t (:foreground "maroon"))))
 '(italic ((t (:foreground "red4" :size "12"))))
 '(jde-java-font-lock-bold-face ((t (:foreground "firebrick"))))
 '(jde-java-font-lock-italic-face ((t nil)))
 '(message-cited-text ((t nil)))
 '(message-header-contents ((t nil)))
 '(mode-line ((t (:size "12pt" :family "Clean" :foreground "Black" :background "gainsboro"))))
 '(mode-line-buffer-id ((t (:foreground "blue4" :background "gainsboro"))))
 '(modeline-mousable ((t (:foreground "firebrick" :background "gainsboro"))) t)
 '(modeline-mousable-minor-mode ((t (:foreground "green4" :background "gainsboro"))) t))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(TeX-one-master "<none>")
 '(TeX-view-style (quote (("^a5$" "kdvi %d -paper a5") ("^landscape$" "kdvi %d -paper a4r -s 4") ("." "kdvi %d"))))
 '(blink-cursor-mode nil)
 '(c-basic-offset 4 t)
 '(column-number-mode t)
 '(delete-key-deletes-forward t t)
 '(gnuserv-frame t)
 '(gnuserv-visit-hook (quote (raise-frame)))
 '(inhibit-startup-screen t)
 '(load-home-init-file t t)
 '(progress-feedback-use-echo-area (quote t))
 '(query-user-mail-address nil)
 '(scroll-bar-mode (quote right))
 '(user-mail-address (concat "klee" "@" "users.sourceforge.net"))
 '(visible-bell t)
 '(zmacs-regions nil))

