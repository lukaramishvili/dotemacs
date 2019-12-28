;;;; package --- Summary
;;;; Luka Ramishvili's .emacs file
;;;;

;;; Commentary:

;;; use C-x C-e to reload any s-exp

;;; Code:

(cd "/projects/")

(setq default-directory "/projects/")

(setq mac-command-modifier 'control)
(setq mac-control-modifier 'super)
;; there's also 'control (C-), 'meta (M-), 'super (S-) and 'hyper (H-)
(global-set-key [(super h)] 'help-command)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)


;;; packages

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(setq needed-packages-list
      '(
        ;;; required packages to install on a new system:
        use-package
         free-keys
         hungry-delete
         exec-path-from-shell
         recentf-ext
         editorconfig
         magit
         web-mode
         scss-mode
         emmet-mode
         ;;diredful
         org
         paredit
         ;; dims parens visually, very useful in lisp code
         ;;27decdisableforperf;;paren-face
         ;;; optional packages
         php-mode
         slime
         slime-repl-ansi-color
         slime-company
         ;; got CPU to 100 (without any images) and had to force-quit. don't really need every day.
         ;; slime-docker
         clojure-mode
         clojure-mode-extra-font-locking
         cider
         ido-completing-read+
         smex
         projectile
         ;;27decdisableforperf;;rainbow-delimiters
         tagedit
         ;;
         ensime
         haskell-mode
         sclang-extensions
         ;;27decdisableforperf;;flymd
         ;;27decdisableforperf;;markdown-mode
         ;;27decdisableforperf;;markdown-preview-mode
         ;;; asciidoc mode
         adoc-mode
         ;;; debugger
         ;;27decdisableforperf;;dap-mode
         ;;; Language Server Protocol support
         ;;27decdisableforperf;;lsp-mode
         ;;27decdisableforperf;;lsp-ui ;; flycheck integration and higher level UI modules
         ;;27decdisableforperf;;company
         ;;27decdisableforperf;;company-lsp ;; for lsp-mode's company-mode integration. don't forget to uncomment its use-package above
         ;;; Angular
         ;;27decdisableforperf;;ng2-mode ;; will bring typescript-mode
         ;;27decdisableforperf;;tide ;; typescript interactive devenv
         ;;27decdisableforperf;;ts-comint ;; ts REPL; requires `sudo npm i -g tsun`
         ;; React
         ;;27decdisableforperf;;rjsx-mode
         ;;27decdisableforperf;;flow-js2-mode ;; flow support in js2-mode
         ;;27decdisableforperf;;flycheck-flow ;; for flow support in flycheck
         ;;27decdisableforperf;;flow-minor-mode ;; for flow support in flycheck
         ;;27decdisableforperf;;company-flow ;; company support for flow
         ;; flycheck / eslint
         ;;27decdisableforperf;;flycheck
         ;;27decdisableforperf;;add-node-modules-path
         ;;27decdisableforperf;;prettier-js ;; don't forget to `npm i -g prettier`
         ;;; w3m needed for SuperCollider help system
         w3m
         ;;
         omnisharp))
;; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package needed-packages-list)
  (unless (package-installed-p package)
    (package-install package)))



;;; installed packages (copied from M-x package-list-packages)
;;;company - autocompletion (not using, too cumbersome and not at all useful)
;;;js2-mode - for modern javascript files (painfully slow)



(require 'use-package)

(require 'thingatpt)

;(require 'magit)

(require 'editorconfig)
(editorconfig-mode 1)





(defun bool (arg)
  "Convert ARG to boolean value."
  (not (not arg)))

(defun os ()
  "Return the current operating system type – mac, windows or linux."
  (cond
   ((string-equal system-type "windows-nt") ; Microsoft Windows
    'windows)
   ((string-equal system-type "darwin") ; Mac OS X
    'mac)
   ((string-equal system-type "gnu/linux") ; linux
    'linux)))

(defun os-is (os-asked)
  "Return true if the current operating system matches OS-ASKED."
  (eq (os) os-asked))

;; Sets up exec-path-from shell
;; https://github.com/purcell/exec-path-from-shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH")))

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

(defun eshell-here ()
  "Open a new shell in the directory associated with the current buffer's file.
The directory name is added to window name to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

;;(global-set-key (kbd "C-!") 'eshell-here)

(defun eshell/exec (command)
  "Execute command COMMAND in Eshell."
  (insert command)
  (eshell-send-input))

(defun eshell/x ()
  "Close the eshell window."
  (insert "exit")
  (eshell-send-input)
  (delete-window))


;; from Tikhon Jelvis (modified to include bash profile)
(defun new-shell (name)
  "Open a new shell buffer with the given name in asterisks (*NAME*) in the current directory and change the prompt to 'name>'."
  (interactive "sName: ")
  ;; pop-to-buffer caused more headaches than I can count, opening the shell in different buffers and messing with window layout. switch-to-buffer opens the shell in the current window, so the problem's gone.
  (switch-to-buffer (concat "*" name "*"))
  (unless (eq major-mode 'shell-mode)
    (shell (current-buffer))
    (sleep-for 0 200)
    (delete-region (point-min) (point-max))
    ;; set prompt name && include user bash profile
    (comint-simple-send (get-buffer-process (current-buffer)) 
                        (concat "export PS1=\"\033[33m" name "\033[0m:\033[35m\\W\033[0m>\" && source ~/.bash_profile"))
    (set-windmove-keybindings)))
(global-set-key (kbd "C-c s") 'new-shell)


(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed."
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to exit Emacs? "))
      (if (< emacs-major-version 22)
          (save-buffers-kill-terminal)
        (save-buffers-kill-emacs))
    (message "Canceled exit")))

;; maximize emacs frame on startup (X11-specific but I'm not using anything else)
(defun x11-maximize-frame ()
  "Maximize the current frame (to full screen)."
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0)))
;;(x11-maximize-frame)

(defun insert-double-quotes (&optional arg)
  "Wrap following ARG sexps in double quotes.
If ARG is 0, insert double quotes and place the cursor between them."
  (interactive "P")
  (insert-pair arg ?\" ?\"))

(defun insert-square-brackets (&optional arg)
  "Wrap following ARG sexps in square brackets.
If ARG is 0, insert square brackets and place the cursor between them."
  (interactive "P")
  (insert-pair arg ?\[ ?\]))
 
  ;; (insert "\"")
  ;; (save-excursion
  ;;   (insert "\"")))


;; function 'previous-mode is already defined. other-window will throw error if you redefine this
(defun switch-to-previous-window ()
  "Switch to previous window in the current frame."
  (interactive)
  (other-window -1))

(defun inside-string? ()
  "Return non-nil if inside string, else nil.
This depends on major mode having setup syntax table properly."
  (interactive)
  (let ((result (nth 3 (syntax-ppss))))
    (message "%s" result)
    result))

;; (fset 'original-backward-up-list (symbol-function 'backward-up-list))
;; (defun backward-up-list (&optional arg escape-strings no-syntax-crossing)
;;   (interactive)
;;   (cond
;;    ((equal (inside-string?) 34)
;;     (search-backward "\"")); get out of "" string
;;    ((equal (inside-string?) 39)
;;     (search-backward "'")))
;;   (original-backward-up-list))



(require 'hungry-delete)

;;(use-package hungry-delete
;;             :bind (("<backspace>" . hungry-delete-backward)
;;                    ("C-S-d" . hungry-delete-backward)
;;                    ("C-h" . hungry-delete-backward)
;;                    ("C-d" . hungry-delete-forward)))


;; variations on Steve Yegge recommendations
(defun kill-current-word ()
  (interactive)
  ;;in case cursor was at the end of current word, prevent jumping to next word's end
  (left-word 1)
  (right-word 1)
  (backward-kill-word 1))
(defun kill-current-symbol ()
  "Kill atomic sexp at cursor."
  (interactive)
  (backward-sexp 1)
  (kill-sexp 1))
(defun kill-current-line ()
  "Kill current line including the newline at the end."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line 1))
(defun join-with-next-line (&optional leave-blank-lines)
  (interactive)
  (move-end-of-line 1)
  ;; delete blank lines before moving to next line, or an empty line can remain
  (if (not leave-blank-lines) (delete-blank-lines))
  (forward-char 1)
  (delete-indentation)
  ;; sometimes delete-indentation leaves one space, so delete that
  (if (equal " " (string (char-after (point))))
      (delete-char 1)))
(defun kill-current-sexp ()
  (interactive)
  (backward-up-list)
  (kill-sexp))
(defun kill-and-join-forward (&optional arg)
  "If cursor's at end of line, join with following line.
If not, kill ARG lines.
Also deletes whitespace at join."
  (interactive "P")
  (kill-line arg)
  (hungry-delete-forward 0))
;; this is a reverse of C-k (deletes line contents *before* cursor)
(defun backward-kill-line ()
  "Kill the contents from cursor to start of line."
  (interactive)
  (set-mark-command nil)
  (move-beginning-of-line 1)
  (backward-delete-char-untabify 1))

(defun whitespacep (c)
  "Return true if character C is whitespace."
  (bool
   (cond ((characterp c) (or (char-equal c #x9)
                             (char-equal c #xa)
                             (char-equal c #x20)))
         ((stringp c) (or (equal c "\t")
                          (equal c "\n")
                          (equal c " ")))
         (t nil))))

(defun kill-whitespace-around-cursor ()
  "Kill all whitespace surrounding cursor, including newlines."
  (interactive)
  (if (whitespacep (preceding-char))
      (hungry-delete-backward 0))
  (if (whitespacep (following-char))
      (hungry-delete-forward 0)))

;; kills all whitespace around cursor and leaves only one space
(defun just-one-whitespace ()
  "Delete all whitespace around cursor, including newlines, and leave exactly one space."
  (interactive)
  (kill-whitespace-around-cursor)
  (insert " "))

(defun kill-whitespace-around-line ()
  "Place the current line's contents between the non-whitespace characters found on surrounding lines."
  (interactive)
  (move-beginning-of-line 1)
  (kill-whitespace-around-cursor)
  (just-one-space)
  (move-end-of-line 1)
  (kill-whitespace-around-cursor)
  (just-one-space))

(defun add-whitespace-around-line ()
  "Add empty lines around the current line."
  (interactive)
  (move-beginning-of-line 1)
  (newline-and-indent)
  (move-end-of-line 1)
  (newline-and-indent))

;; M-S-space: useful for just formatting a single-line block with no intention of adding code
(defun add-whitespace-around-block (&optional add-extra-line-p)
  "Place { or ( current block contents } or ) on a separate line.
If ADD-EXTRA-LINE-P, add preceding empty line and open a new line below for new code."
  (interactive)
  ;; find enclosing paren or bracket and position cursor right after it
  (backward-up-list)
  (down-list)
  ;; add line before block contents
  (newline-and-indent)
  ;; find ending paren (positioning the cursor after ending paren/bracket)
  (backward-up-list)
  (forward-list)
  ;; position cursor right before the ending paren/bracket
  (backward-char)
  ;; add newline before ending paren or bracket
  (newline-and-indent)
  (when add-extra-line-p
    ;; add an extra line on which the ending paren will rest, but leave the cursor on the empty line
    (open-line 1)
    ;; move to the next line, just before the ending paren/bracket
    (forward-char)
    ;; indent the ending paren line
    (indent-for-tab-command)
    ;; return to blank line before ending paren/bracket
    (move-beginning-of-line 1)
    (backward-char)
    ;; indent that blank line
    (indent-for-tab-command))
  ;; if not adding anything to block contents (only formatting the block),then position the cursor after ending paren
  (unless add-extra-line-p
    (forward-char)))

;; M-S-return: useful for expanding a single-line block when intending to append more code to the block content
(defun add-whitespace-around-block-and-newline ()
  "Place { or ( current block contents } or ) on a separate line and open a new line below for new code."
  (interactive)
  (add-whitespace-around-block t))

(defun comment-line ()
  "Comment current line entirely."
  (interactive)
  (move-beginning-of-line 1)
  (set-mark-command nil)
  (move-end-of-line 1)
  ;;will call comment-region
  (comment-dwim nil))

;; this one is basically the same but from emacs-for-clojure-book
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(global-set-key (kbd "C-M-;") 'comment-line)

(global-set-key (kbd "C-;") 'toggle-comment-on-line)

;; C-return (adds an indented line after current line and moves cursor there) is overridden by emmet-mode
(defun open-indented-line ()
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))
;; o-i-b / Command-Shift-Enter adds two lines, cursor on first (useful in <div></div>)
(defun open-indented-block ()
  (interactive)
  (newline-and-indent)
  (newline-and-indent)
  (previous-line 1)
  (indent-for-tab-command))
;; Command-{ opens a {\n cursor will be here \n} block after the end of the line
;; if at-the-end-of-line is nil, opens the {\n cursor \n} block at the cursor position
(defun open-brackets-block (at-the-end-of-line)
  (interactive)
  (if at-the-end-of-line (move-end-of-line 1))
  (insert "{")
  (newline-and-indent)
  (insert "}")
  (indent-for-tab-command)
  (previous-line 1)
  (open-indented-line))
;; Command-Alt-{ inserts "{ <cursor> }"
(defun open-brackets-block-inline ()
  (interactive)
  (insert "{ ")
  (save-excursion
    (insert " }")))
;; Command-} closes a {} block on a new line (e.g. if or while), cursor will be after }
;; Command-M-} closes a {} block right after cursor, cursor will be before }
(defun close-brackets-block (at-the-end-of-line)
  (interactive)
  (if at-the-end-of-line (move-end-of-line 1))
  (newline-and-indent)
  (insert "}")
  (unless at-the-end-of-line (backward-char 1))
  (indent-for-tab-command))
(defun open-js-lambda-block ()
  "Insert anonymous function at cursor."
  (interactive)
  (insert "function()")
  (open-brackets-block nil))

(defun sgml-transpose-tags-around-cursor ()
  "Transpose tags before and after cursor."
  (interactive)
  ;; transpose only works when cursor is at first element's beginning
  (sgml-skip-tag-backward 1)
  (web-mode-element-transpose))
(defun sgml-delete-tag-backward (arg)
  "Delete the tag before cursor."
  (interactive "p")
  (let ((tag-start (point)))
    (sgml-skip-tag-backward arg)
    (kill-region tag-start (point))))
(defun sgml-delete-tag-forward (arg)
  "Delete the tag before cursor."
  (interactive "p")
  (let ((tag-end (point)))
    (sgml-skip-tag-forward arg)
    (kill-region tag-end (point))))
(defun sgml-select-tag-backward (arg)
  "Select ARG tags before cursor (includes any space between current cursor position and closing tag."
  (interactive "p")
  (set-mark-command nil)
  (sgml-skip-tag-backward arg))
(defun sgml-select-tag-forward (arg)
  "Select ARG tags before cursor (includes any space between current cursor position and opening tag."
  (interactive "p")
  (set-mark-command nil)
  (sgml-skip-tag-forward arg))
(defun sgml-duplicate-previous-tag (arg)
  "Duplicate ARG tags before cursor and place them at the current cursor position."
  (interactive "p")
  ;; remember the current cursor position; we'll paste there
  (save-excursion
    ;; jump to the beginning of previous tag, select it, and copy
    (sgml-skip-tag-backward arg)
    (set-mark-command nil)
    (sgml-skip-tag-forward arg)
    (kill-ring-save (point) (mark)))
  (open-indented-line)
  (yank))
(defun sgml-duplicate-next-tag (arg)
  "Insert the contents of the following ARG tags after cursor at the current cursor position."
  (interactive "p")
  ;; remember the current cursor position; we'll paste there
  (save-excursion
    ;; jump to the beginning of previous tag, select it, and copy
    (sgml-skip-tag-forward arg)
    (set-mark-command nil)
    (sgml-skip-tag-backward arg)
    (kill-ring-save (point) (mark)))
  (open-indented-line)
  (yank))
(defmacro sgml-with-tag-contents-after-cursor (&rest body)
  "Manipulate the contents of the following sgml tag by executing the macro's BODY."
  `(progn
     ;; TODO: we need to somehow avoid matching closing tags, which also contain ">"'s.
     ;; if we simply search for ">", this code will fail when the cursor is inside a closing tag or before a tag with children (e.g. </di|v> or <di|v> <div>contents</div> </div>, respectively)
     (search-forward ">")
     (set-mark-command nil)
     (search-forward "<")
     (backward-char 1)
     ,@body))
(defun sgml-clean-tag-after-cursor ()
  "Delete the contents of the following sgml tag."
  (interactive)
  (sgml-with-tag-contents-after-cursor
   (delete-region (region-beginning) (region-end))))
(defun sgml-kill-tag-contents-after-cursor ()
  "Kill the contents of the following sgml tag."
  (interactive)
  (sgml-with-tag-contents-after-cursor
   (kill-region (region-beginning) (region-end))))



;;; settings

;; hide annoying GNU ad (I thereby classify it as such)
(setq inhibit-startup-message t)
;; clear *scratch* default contents
(setq initial-scratch-message nil)

;; for ispell
(setenv "DICTIONARY" "en_US")


(setq ring-bell-function 'ignore)

;; disabled guru mode; doesn't play well with multiple input sources (need arrows with non-english layouts)
;; (add-to-list 'load-path "~/.emacs.d/guru-mode")
;; (require 'guru-mode)
;; (guru-global-mode +1)

;; line numbers
(global-linum-mode);; -1 for off

;; Highlight current line
(global-hl-line-mode 1)

;; turns off cursor blink.
;; I prefer blinking cursor – easier to find, but highlight-line-mode might solve that.
(blink-cursor-mode 0)


;;; colors
;;(set-background-color "#3f3f3f")
;;(set-foreground-color "white")


(defun load-eink-theme ()
  (load-theme 'eink t)
  (global-hl-line-mode -1)
  ;; goes well with eink theme
  (set-default-font "Iosevka-16"))

;; themes
(add-to-list 'custom-theme-load-path "~/dotemacs/themes")
(add-to-list 'load-path "~/dotemacs/themes")
;; (load-theme 'blackboard t)
;; (load-theme 'tomorrow-night-bright t)
;; (load-theme 'tomorrow-night-blue t)
;; (load-theme 'tomorrow-night-eighties t)
;; (load-theme 'tomorrow-night t)
(load-theme 'zenburn t)
;;(load-eink-theme)

;; TODO try multiple fonts for code and strings:
;; https://bastibe.de/2017-09-19-multi-font-themes.html

;; increase font size for better readability
(set-face-attribute 'default nil :height 140)

(defun font-installed-p (font)
  "Return true if the font is installed."
  (bool (x-list-fonts font)))

;;; fonts
(if (font-installed-p "DejaVu Sans Mono")
    (set-default-font "DejaVu Sans Mono-14"))

;; tried out and immediately turned off.
;; (if (font-installed-p "Input Mono Narrow")
;;     (set-default-font "Input Mono Narrow"))

;; (if (font-installed-p "SF Mono")
;;      (set-default-font "SF Mono-16"))

;; a very beautiful font.
;; (if (font-installed-p "Iosevka")
;;     (set-default-font "Iosevka-16"))

;; a great 90's style font for long hours spent staring at the monitor
(if (font-installed-p "Terminus (TTF)")
  (set-default-font "Terminus (TTF)-18"))

;; https://bastibe.de/2017-09-19-multi-font-themes.html

;; Color Themes
;; Read http://batsov.com/articles/2012/02/19/color-theming-in-emacs-reloaded/
;; for a great explanation of emacs color themes.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Custom-Themes.html
;; for a more technical explanation.
;; (add-to-list 'custom-theme-load-path "~/dotemacs/themes")
;; (add-to-list 'load-path "~/dotemacs/themes") ;
;; (load-theme 'tomorrow-night-bright t)

;; disable tab indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; when autocompleting directories, you really want to autocomplete Http when typing h.
(setq completion-ignore-case  t)

;; enable C-x C-u and C-x C-l (for upcasing/downcasing selection/region)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; always follow symlinks (avoid annoying yes/no question)
(setq vc-follow-symlinks t)

;; Key binding to use "hippie expand" for text autocompletion
;; http://www.emacswiki.org/emacs/HippieExpand
(global-set-key (kbd "M-/") 'hippie-expand)

;; Lisp-friendly hippie expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;;; from better-defaults
(global-set-key (kbd "s-SPC") 'hippie-expand)
(global-set-key (kbd "s-S-SPC") 'ispell-complete-word)

(global-set-key (kbd "M-!") 'shell-command)
(global-set-key (kbd "M-&") 'async-shell-command)
(global-set-key (kbd "s-!") 'async-shell-command);; don't use C-!, it's used to yank without formatting.

(global-set-key (kbd "M-#") 'query-replace)
(global-set-key (kbd "M-$") 'replace-string)
;; (global-set-key (kbd "M-%") 'ispell-word)
(global-set-key (kbd "M-%") 'replace-regexp)

 (autoload 'zap-up-to-char "misc"
   "Kill up to, but not including ARGth occurrence of CHAR." t)
(defun zap-up-to-char-add-newline (arg char)
  "Kill up to, but not including ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.
Ignores CHAR at point, and also ignores."
  (interactive "p\ncZap up to char, then add newline: ")
  (zap-up-to-char arg char)
  (newline-and-indent))
(global-set-key (kbd "M-S-z") 'zap-up-to-char)
(global-set-key (kbd "M-z") 'zap-up-to-char-add-newline)

;; NOT WORKING; BTT wouldn't allow any combination of disabling/replacing/retaining/overriding keypress combinations for Emacs, so I opted to add this keybinding to apps one by one. the there's also the possibility to map M-<> to CMD-Page Up/Down which is basically the same as CMD-up/down.
;; [incorrect] use M-< and M-> system-wide; BetterTouchTool has no excludes, so listen for cmd-up/down to maintain M-</> behavior
;; [incorrect] BUT, REAL keypresses of C-up/down/arrows are used for navigating between windows, so I've remapped their REAL presses to super-P/super-N in BetterTouchTool.
;; (global-set-key (kbd "<C-up>") 'beginning-of-buffer)
;; (global-set-key (kbd "<C-down>") 'end-of-buffer)

(require 'company)
;; aligns annotation to the right hand side
;; needed for tide (Typescript IDE mode)
(setq company-tooltip-align-annotations t)

;; (require 'prettier-js)
;; I'm already loading tide-mode with tons of functionality, so just comment and leave prettier-js for later.
;; (add-hook 'ng2-ts-mode-hook 'prettier-js-mode)
;; (add-hook 'typescript-mode-hook 'prettier-js-mode)


;; (load "~/dotemacs/javascript.el")
(defun setup-javascript ()
  "Setup and configure javascript packages, keybindings, etc."
  (interactive)
  (load "~/dotemacs/javascript.el"))

(defun setup-javascript-mode ()
  (setup-javascript))


;; M-x `compile tsc format`
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  "Colorize the compilation buffer."
  (ansi-color-apply-on-region compilation-filter-start (point-max)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)


;; no hook for tetris-mode
;; (defun setup-tetris-mode ()
;;   "Setup tetris mode Emacs keybindings."
;;   (local-set-key (kbd "C-f") 'tetris-move-right)
;;   (local-set-key (kbd "C-b") 'tetris-move-left)
;;   (local-set-key (kbd "C-p") 'tetris-rotate-next)
;;   (local-set-key (kbd "C-n") 'tetris-move-bottom))
;; (add-hook 'tetris-mode-hook 'setup-tetris-mode)

(defvar tetris-mode-map
  (make-sparse-keymap 'tetris-mode-map))

(define-key tetris-mode-map "f"     'tetris-move-right)
(define-key tetris-mode-map "b"     'tetris-move-left)
(define-key tetris-mode-map "p"     'tetris-rotate-next)
(define-key tetris-mode-map "n"     'tetris-move-down)
(define-key tetris-mode-map " "     'tetris-move-bottom)
(define-key tetris-mode-map "v"     'tetris-move-bottom)
(define-key tetris-mode-map (kbd "RET")     'tetris-move-bottom)


;; display project1/samename.js instead of samename.js<project1>
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; a better buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; kill buffer immediately on C-x k.
;; thought may revert this because I also have ESC-k, but the reason I reverted immediately is, turns out, C-x k RET is already tatooed in muscle memory (did it after closing). Thanks, timid guy who added a confirmation buffer.
;;(global-set-key (kbd "C-x k") 'kill-current-buffer)

;; Enhances M-x to allow easier execution of commands. Provides
;; a filterable list of possible commands in the minibuffer
;; http://www.emacswiki.org/emacs/Smex
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; Hyphen on Space
;; Modify smex so that typing a space will insert a hyphen ‘-’ like in normal M-x.
(defadvice smex (around space-inserts-hyphen activate compile)
  (let ((ido-cannot-complete-command 
         `(lambda ()
            (interactive)
            (if (string= " " (this-command-keys))
                (insert ?-)
              (funcall ,ido-cannot-complete-command)))))
    ad-do-it))
;; Update less often
(defun smex-update-after-load (unused)
  (when (boundp 'smex-cache)
    (smex-update)))
(add-hook 'after-load-functions 'smex-update-after-load)


;; projectile everywhere!
(projectile-global-mode)

(global-set-key (kbd "C-x C-g") 'goto-line)

(show-paren-mode 1)
(setq-default indent-tabs-mode nil)
(setq save-interprogram-paste-before-kill t
      apropos-do-all t
      ;;mouse-yank-at-point t
      require-final-newline t
      ;; visible bell is aweful.
      ;; visible-bell t
      load-prefer-newer t
      ediff-window-setup-function 'ediff-setup-windows-plain
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))


;; enable sourcing .bashrc files in 'shell-command (M-!)
;; from https://stackoverflow.com/a/12229404/324220
(setq shell-file-name "bash")
;; (setq shell-command-switch "-ic")
;; after updating to Bash 5, had to disable -i interactive mode, which requires a terminal emulator not available in M-!
;; if only the "-c" switch was used, that would disable auto-sourcing .bashrc.
;; so we're using a login shell instead of an interactive shell by specifying "-lc" (this method requires adding "shopt -s expand_aliases" to .bash_profile)
;; from https://emacs.stackexchange.com/a/5975/15260
(setq shell-command-switch "-lc")

;; doesnt work to also enable it in eshell-mode
;; (add-hook 'eshell-mode-hook  (lambda () (eshell/exec "source ~/dotemacs/.bashrc")))

(autoload 'comint-dynamic-complete-filename "comint" nil t)
(global-set-key (kbd "s-\\") 'toggle-input-method)
(global-set-key (kbd "C-\\") 'comint-dynamic-complete-filename)


;;show free keybindings on s-h s-k
(require 'free-keys)
(global-set-key (kbd "s-h s-k") 'free-keys)

;; didn't do anything by default and didn't find it useful.
;;(require 'diredful)
;;(diredful-mode 1)



;; Find file in current directory:
(global-set-key (kbd "C-M-,") 'find-file-in-current-directory)

(when window-system
  (global-set-key (kbd "C-x C-c") 'ask-before-closing)
  (global-set-key (kbd "s-q") 'ask-before-closing))

;; stops selection with a mouse being immediately injected to the kill ring
(setq mouse-drag-copy-region nil)
;; hide the toolbar (check if available, or signals error in terminal)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;; hide the menu except on macOS, where the menu doesn't take up space. Not that I use it, though.
(unless (os-is 'mac)
  (if (fboundp 'menu-bar-mode) (menu-bar-mode -1)))
;; hide the scrollbars, not using them anyway
;; also check if available, or signals error in terminal
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;; show column numbers
(column-number-mode)


;; switch () and [] (now system-wide using Karabiner-Elements)
;;(keyboard-translate ?\( ?\[)
;;(keyboard-translate ?\[ ?\() 
;;(keyboard-translate ?\) ?\]) 
;;(keyboard-translate ?\] ?\))
;; Also use Alt-[ as Alt-( and Alt-] as Alt-)
(global-set-key (kbd "M-[") 'insert-parentheses)
(global-set-key (kbd "M-]") 'move-past-close-and-reindent)
(global-set-key (kbd "M-(") 'insert-square-brackets)
(global-set-key (kbd "M-)") 'move-past-close-and-reindent)


(global-set-key (kbd "M-\"") 'insert-double-quotes)

(global-set-key (kbd "C-x i") 'switch-to-previous-window)
(global-set-key (kbd "C-x O") 'switch-to-previous-window)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-O") 'switch-to-previous-window)
;;;navigate between windows using C-*-tab
(global-set-key (kbd "<C-tab>") 'other-window)
(global-set-key (kbd "<C-S-tab>") 'switch-to-previous-window)
(global-set-key (kbd "<C-S-iso-lefttab>") 'switch-to-previous-window)
(global-set-key (kbd "M-±") 'switch-to-previous-window);; same as M-S-§
(global-set-key (kbd "M-§") 'other-window)

(global-set-key (kbd "<C-M-tab>") 'next-buffer)
(global-set-key (kbd "<C-M-S-tab>") 'previous-buffer)

;; ctrl-tab / ctrl-shift-tab on Mac keyboard
(global-set-key (kbd "<s-tab>") 'switch-to-previous-window)
(global-set-key (kbd "<S-s-tab>") 'other-window)

;; OLD: in web/html-mode, M-n/M-p navigates between tags, so add another binding
;; NEW: new frame should have a consistent keybinding, and I'm not really using M-n/M-p, instead using C-c C-f/b
;; (global-set-key (kbd "M-s-n") 'new-frame)
;;(global-set-key (kbd "M-S-n") 'new-frame)
(global-set-key (kbd "<s-backspace>") 'delete-window)

(defun new-temp-buffer ()
  (interactive)
  (switch-to-buffer (format "tmp-%s" (random 100))))

;; create a temporary buffer with a random name
(global-set-key (kbd "C-x t") 'new-temp-buffer)

;; BetterTouchTool's "C-f => <right arrow>" lingers when switching to Emacs.
;; It's too slow to turn off the other app's keybindings and turns C-f into right arrow. This happens for <=1s max.
;; Luckily, Emacs is an editor and I would be very careful to perform any operation in an editor immediately after switching to it.
;; Except C-x C-f, which is a non-mutating operation (opening files never hurt nobody), and which, turns out, I use immediately after switching to Emacs, 10 times a minute.
;; So make an exception for C-x <right> and hope that I don't need any more
;; * destructive keybindings
;; * which are bound by BTT and are as essential as C-f
;; * which I'll need _immediately_ after switching to Emacs.
(global-set-key (kbd "C-x <right>") 'find-file)

;; CMD-Shift-backspace
(global-set-key (kbd "<C-S-backspace>") 'backward-kill-line)
;; CMD-ctrl-K
(global-set-key (kbd "C-s-K") 'backward-kill-line)
(global-set-key (kbd "C-M-h") 'backward-kill-sexp)
(global-set-key [(meta shift h)] 'backward-kill-sexp);; (kbd "M-S-h") didn't work.
(global-set-key (kbd "<C-backspace>") 'backward-kill-sexp)
(global-set-key (kbd "<C-M-backspace>") 'backward-kill-sexp)
(global-set-key (kbd "<M-S-backspace>") 'backward-kill-sexp)

;; make C-h backspace, and use super-h for help (ctrl-h on my Mac)
(global-set-key "\C-h" 'backward-delete-char-untabify)
;; commented: use C-S-d as backspace
;; (global-set-key (kbd "C-S-d") 'backward-delete-char-untabify)
;; adding Shift to C-h or C-d hungry-deletes (deletes all whitespace it meets)
(global-set-key (kbd "C-S-h") 'hungry-delete-backward)
;;C-backspace was backward-kill-word, but I already have C-1,M-h,C-M-h,M-DEL for that
(global-set-key (kbd "<C-backspace>") 'hungry-delete-backward)
(global-set-key (kbd "<S-backspace>") 'hungry-delete-backward)
;;(global-set-key (kbd "<C-S-backspace>") 'hungry-delete-backward)
(global-set-key (kbd "C-S-d") 'hungry-delete-forward)
;; also use C-h for backspace in regex search
(define-key isearch-mode-map "\C-h" 'isearch-delete-char)


;; select whole line. stumbled upon a complete solution while looking for how to implement shift-selection.
;; (source: http://emacs.stackexchange.com/a/22166/93)
(defun mark-current-line (&optional arg)
  "Uses shift selection to select the current line.
When there is an existing shift selection, extends the selection
in the appropriate direction to include current line."
  (interactive "p")
  (let ((oldval (or (cdr-safe transient-mark-mode) transient-mark-mode))
        (backwards (and mark-active (> (mark) (point))))
        (beg (and mark-active (mark-marker))))
    (unless beg
      (if backwards (end-of-line) (beginning-of-line))
      (setq beg (point-marker)))
    (if backwards (end-of-line (- 1 arg)) (beginning-of-line (+ 1 arg)))
    (unless mark-active
      (push-mark beg nil t))
    (setq transient-mark-mode (cons 'only oldval))))
(global-set-key (kbd "C-S-l") 'mark-current-line)

;;I'm almost always using M-BACKSPACE, so let's use C-1 as yank, which (C-y) is inconvenient
;;(global-set-key (kbd "C-1") 'backward-kill-word)
(global-set-key (kbd "C-2") 'kill-current-word)
(global-set-key (kbd "C-3") 'kill-current-symbol)
(global-set-key (kbd "C-4") 'kill-current-line)
(global-set-key (kbd "C-5") 'kill-current-sexp)
(global-set-key (kbd "C-S-k") 'kill-and-join-forward)
;; C-x C-k bindings are used for keymacro definition
;(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'join-with-next-line)
(global-set-key (kbd "C-c C-M-k") '(lambda () (interactive) (join-with-next-line t)))
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
;; make Alt-h and Alt-Ctrl-h the same as Alt-Backspace
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "C-M-h") 'backward-kill-word)





;;; Asciidoc. don't forget `brew install asciidoc`. Docs: https://asciidoctor.org/docs/user-manual/
;;; To convert to HTML5 (default asciidoc output), use `asciidoc file.adoc` (will output file.html)
;;; a2x (Asciidoc to *) behaved weirdly, ran xmllint which detected a2x's own errors. `a2x a2x --format=xhtml [same as -f] --no-xmllint [same as -L]`
;;; pandoc has no Asciidoc reader (so cannot convert *from* Asciidoc).


;;; Markdown

;;27decdisableforperf;;(require 'markdown-mode)
;;27decdisableforperf;;(require 'markdown-preview-mode)

;; open flymd's live-reload in Firefox
(defun my-flymd-browser-function (url)
  "Open flymd's preview URL in Firefox -- Chrome restricts local file access."
  (let ((process-environment (browse-url-process-environment)))
    (apply 'start-process
           (concat "firefox " url)
           nil
           "/usr/bin/open"
           (list "-a" "firefox" url))))
(setq flymd-browser-open-function 'my-flymd-browser-function)

;; for ruby-based markdown-preview-mode. requires `brew install pandoc`
(setq markdown-command "/usr/local/bin/pandoc")

(add-hook 'markdown-mode-hook 'markdown-mode-settings)

;;(setq markdown-preview-function 'flymd-flyit)
(fset 'markdown-preview-function 'markdown-preview-mode)
(defun markdown-mode-settings ()
  "Settings for markdown-mode."
  ;; each call to preview creates new websocket server, so don't auto-preview
  ;; (markdown-preview-function)
  (local-set-key (kbd "C-c C-p") (lambda ()
                                     (interactive)
                                     (markdown-preview-function))))

;; enable ffap. also will autofill C-x C-f and C-x C-d with path from ffap-file-at-point.
(ffap-bindings)
(global-set-key (kbd "C-x C-f") 'find-file)


(global-set-key (kbd "C-M-\\") 'kill-whitespace-around-cursor)
(global-set-key (kbd "<C-M-SPC>") 'just-one-whitespace)

(global-set-key (kbd "C-M-|") 'kill-whitespace-around-line)

(global-set-key (kbd "C-M-S-SPC") 'add-whitespace-around-line)
(global-set-key (kbd "M-S-SPC") 'add-whitespace-around-block)
(global-set-key (kbd "<M-S-return>") 'add-whitespace-around-block-and-newline)


(global-set-key (kbd "C-z") 'undo)
;; these keys are close to and frequently mistyped as the undo sequence, C-/
(global-set-key (kbd "C-.") 'undo)
(global-set-key (kbd "C--") 'undo)

(defun preferences ()
  "\"Preferences\", macOS style. Editing .emacs should be straightforward – I need it every day."
  (interactive)
  (find-file "~/dotemacs/.emacs"))
(global-set-key (kbd "C-,") 'preferences)
(global-set-key (kbd "C-x ,") 'preferences)

(defun open-referenced-file ()
  "Open the file whose name is at the cursor."
  (interactive)
  ;; a quick hack: nunjucks includes are enclosed in quotes, making them simple s-exps
  ;; for _scss and other static files, just add _ to the start and.scss to the end
  ;; old code: symbol-at-point ;;((path (symbol-name(symbol-at-point))))
  (let ((path (ffap-file-at-point)))
    (if (file-exists-p path)
        (find-file path))))
;; Alt-Enter opens included file name at cursor
(global-set-key (kbd "<M-return>") 'open-referenced-file)
(global-set-key (kbd "M-j") 'open-referenced-file)

;; php mode keybindings
(add-hook 'php-mode-hook 'setup-php-mode)

(defun setup-php-mode ()
  "Setup PHP mode."
  (local-set-key (kbd "<f1>") 'lookup-php-function)
  (local-set-key (kbd "C-<f1>") 'lookup-php-symbol)
  (local-set-key (kbd "C-M-h") 'backward-kill-word))

(defun lookup-php-symbol ()
  "Look up the PHP symbol at cursor in documentation."
  (interactive)
  (let ((symbol (symbol-at-point)))
    (if (not symbol)
        (message "No symbol at point.")
      
      (browse-url (concat "http://php.net/manual-lookup.php?pattern="
                          (symbol-name symbol))))))

(defun lookup-php-function ()
  "Look up the PHP function at cursor in documentation."
  (interactive)
  (let* ((function (symbol-name (or (symbol-at-point)
                                    (error "No function at point"))))
         (buf (url-retrieve-synchronously (concat "http://php.net/manual-lookup.php?pattern=" function))))
    (with-current-buffer buf
      (goto-char (point-min))
      (let (desc)
	(when (re-search-forward "<div class=\"methodsynopsis dc-description\">\\(\\(.\\|\n\\)*?\\)</div>" nil t)
	  (setq desc
		(replace-regexp-in-string
		 " +" " "
		 (replace-regexp-in-string
                  "\n" ""
                  (replace-regexp-in-string "<.*?>" "" (match-string-no-properties 1)))))

	  (when (re-search-forward "<p class=\"para rdfs-comment\">\\(\\(.\\|\n\\)*?\\)</p>" nil t)
	    (setq desc
		  (concat desc "\n\n"
			  (replace-regexp-in-string
			   " +" " "
			   (replace-regexp-in-string
			    "\n" ""
			    (replace-regexp-in-string "<.*?>" "" (match-string-no-properties 1))))))))

	(if desc
	    (message desc)
	  (message "Could not extract function info. Press C-F1 to go the description."))))
    (kill-buffer buf)))
;; end of php mode keybindings





;; install omnisharp server before using csharp-mode
;; https://github.com/OmniSharp/omnisharp-emacs/blob/master/doc/server-installation.md
;; M-x omnisharp-install-server

;; for now, to start a project, cd to the solution directory and run $ dotnet run PROJECTNAME

(eval-after-load
    'company
  '(add-to-list 'company-backends #'company-omnisharp))

(defun my-csharp-mode-setup ()
  (omnisharp-mode)
  (company-mode)
  (flycheck-mode)

  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)
  (c-set-style "ellemtel")
  (setq c-basic-offset 4)
  (setq truncate-lines t)
  (setq tab-width 4)
  (setq evil-shift-width 4)

  ;;csharp-mode README.md recommends this too
  ;;(electric-pair-mode 1)       ;; Emacs 24
  ;;(electric-pair-local-mode 1) ;; Emacs 25

  (local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
  (local-set-key (kbd "C-c C-c") 'recompile))

(add-hook 'csharp-mode-hook 'my-csharp-mode-setup t)








;; C-return is overridden by emmet-mode
(global-set-key (kbd "<C-return>") 'open-indented-line)
(global-set-key (kbd "<C-S-return>") 'open-indented-block)
(global-set-key (kbd "<C-M-return>") 'newline-and-indent)
;; C-S-return is vacant, use it for something
(electric-indent-mode 1);; auto-indent newlines etc

(global-set-key (kbd "C-{") (lambda () (interactive) (open-brackets-block t)))
;(global-set-key (kbd "C-M-{") (lambda () (interactive) (open-brackets-block nil)))
(global-set-key (kbd "C-M-{") (lambda () (interactive) (open-brackets-block-inline)))
(global-set-key (kbd "C-}") (lambda () (interactive) (close-brackets-block t)))
(global-set-key (kbd "C-M-}") (lambda () (interactive) (close-brackets-block nil)))

;; for SuperCollider:
;; first, git clone https://github.com/supercollider/scel
(add-to-list 'load-path "~/dotemacs/scel/el/")
;; don't forget to `mv scel/el/sclang-vars.el.in scel/el/sclang-vars.el`
(setf sclang-runtime-directory "/Applications/SuperCollider.app/Contents/Resources")
(require 'sclang)
;; w3m is loaded in the custom auto-loader above (see 'needed-packages)
(require 'w3m)
;; requires 'sclang-extensions auto-loaded above
;; IMPORTANT: place SuperCollider.app in /Applications/SuperCollider like this: /Applications/SuperCollider/SuperCollider.app
(add-hook 'sclang-mode-hook 'sclang-extensions-mode)


(add-hook 'js-mode-hook #'setup-javascript-mode)
(add-hook 'ng2-ts-mode-hook #'setup-javascript-mode)

(defun set-windmove-keybindings ()
  "Configure windmove keybindings.  Useful for overriding mode-specific keybindings."
  (dolist (key '("<C-left>" "<C-right>" "<C-up>" "<C-down>"))
    (global-unset-key (kbd key))
    (local-unset-key (kbd key)))
  
  (global-set-key (kbd "<C-left>") 'windmove-left)
  (global-set-key (kbd "<C-right>") 'windmove-right)
  (global-set-key (kbd "<C-up>") 'windmove-up)
  (global-set-key (kbd "<C-down>") 'windmove-down)
  
  (global-set-key (kbd "<escape> <left>") 'windmove-left)
  (global-set-key (kbd "<escape> <right>") 'windmove-right)
  (global-set-key (kbd "<escape> <up>") 'windmove-up)
  (global-set-key (kbd "<escape> <down>") 'windmove-down)

  (global-set-key (kbd "M-S-n") 'new-frame)

  (global-set-key [(super b)] 'windmove-left)
  (global-set-key [(super f)] 'windmove-right)
  (global-set-key [(super p)] 'windmove-up)
  ;; super-n was well used on new-frame; now using M-n instead
  (global-set-key [(super n)] 'windmove-down)

  ;; excess keybindings; now using these for system-wide switching
  ;; (global-set-key [(super meta b)] 'windmove-left)
  ;; (global-set-key [(super meta f)] 'windmove-right)
  ;; (global-set-key [(super meta p)] 'windmove-up)
  ;; (global-set-key [(super meta n)] 'windmove-down)
  ;; (global-set-key [(control super b)] 'windmove-left)
  ;; (global-set-key [(control super f)] 'windmove-right)
  ;; (global-set-key [(control super p)] 'windmove-up)
  ;; (global-set-key [(control super n)] 'windmove-down)
  
  (progn
    (require 'shell)
    (define-key shell-mode-map (kbd "<C-left>") 'windmove-left)
    (define-key shell-mode-map (kbd "<C-right>") 'windmove-right)
    (define-key shell-mode-map (kbd "<C-up>") 'windmove-up)
    (define-key shell-mode-map (kbd "<C-down>") 'windmove-down)

    (local-set-key (kbd "<C-left>") 'windmove-left)
    (local-set-key (kbd "<C-right>") 'windmove-right)
    (local-set-key (kbd "<C-up>") 'windmove-up)
    (local-set-key (kbd "<C-down>") 'windmove-down)

    (global-set-key [(super b)] 'windmove-left)
    (global-set-key [(super f)] 'windmove-right)
    (global-set-key [(super p)] 'windmove-up)
    ;; super-n is well used on new-frame, so use super-meta-n
                                        ;(global-set-key [(super n)] 'windmove-down)

    ;; excess keybindings; now using these for system-wide switching
    ;; (global-set-key [(super meta b)] 'windmove-left)
    ;; (global-set-key [(super meta f)] 'windmove-right)
    ;; (global-set-key [(super meta p)] 'windmove-up)
    ;; (global-set-key [(super meta n)] 'windmove-down)
    ;; (define-key shell-mode-map [(control super b)] 'windmove-left)
    ;; (define-key shell-mode-map [(control super f)] 'windmove-right)
    ;; (define-key shell-mode-map [(control super p)] 'windmove-up)
    ;; (define-key shell-mode-map [(control super n)] 'windmove-down))
    ;; don't leave a single paren
    (progn)))

(set-windmove-keybindings)


;;switch buffer now doesn't touch other windows, opens in same buffer
(global-set-key "\C-x\C-b" 'buffer-menu)

;;currently "Control" (corner key labeled fn) is mapped to "Alt" on my macbook, 
;;so I can't use s-`. also, "§" key is above the "Tab" key where ` should be
;; pass -1 to other-frame to switch frames in the order they were opened
(global-set-key (kbd "C-§") '(lambda() (interactive) (other-frame -1)))
(global-set-key (kbd "C-±") 'other-frame)
;;on the external keyboard, there's no § key, just a normal `
;;(global-set-key (kbd "C-`") 'other-frame)


(defun show-recent-file-list()
  "Show the recent file list in current buffer."
  (interactive)
  (recentf-mode)
  (recentf-open-files))
;; only useful without project-mode; but when enabled, messes up project-mode.
;; on load, show recent file list
;;(add-hook 'window-setup-hook 'show-recent-file-list)
(defun recent-files-in-split-window ()
  "Switch to newly split window and show recent files list in it."
  (other-window 1)
  (show-recent-file-list))
; when splitting a window ("tab"), show recent file list
(defun split-and-switch-window-below ()
  "Split to a new window below and show recent files list in it."
  (interactive)
  (split-window-below)
  (recent-files-in-split-window))
(defun split-and-switch-window-right ()
  "Split to a new window on the right and show recent files list in it."
  (interactive)
  (split-window-right)
  (recent-files-in-split-window))
(global-set-key (kbd "C-x 2") 'split-and-switch-window-below)
(global-set-key (kbd "C-x 3") 'split-and-switch-window-right)
; quick shortcuts for invoking recent file list
(global-set-key (kbd "C-x M-f") 'show-recent-file-list)
;; was causing error when installing slime - "Key sequence C-x C-a C-l starts with non-prefix key C-x C-a"
;; (global-set-key (kbd "C-x C-a") 'show-recent-file-list)

(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(setq recentf-max-menu-items 40)

;; ido-mode allows you to more easily navigate choices. For example,
;; when you want to switch buffers, ido presents you with a list
;; of buffers in the the mini-buffer. As you start to type a buffer's
;; name, ido will narrow down the list of buffers to match the text
;; you've typed in
;; http://www.emacswiki.org/emacs/InteractivelyDoThings
(defun ido-keybindings ()
  "Keybindings for ido mode."
  ;; by default, ido-mode uses C-f for reverting to Emacs find-file engine, and C-b for reverting to Emacs switch-buffer engine.
  ;; C-f/b is easier to use for navigation, so I'm swapping them.
  ;; for more information, M-. on any 'ido-* function and search for ido-common-completion-map.
  ;; navigate matches with C-f/C-b.
  (define-key ido-completion-map (kbd "C-f") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-b") 'ido-prev-match)
  ;; revert to Emacs engines with C-r when needed (works on both find-file and switch-buffer)
  (define-key ido-completion-map "\C-r" 'ido-fallback-command)
  ;; also use C-g for fallback, since it feels intuitive
  (define-key ido-completion-map "\C-g" 'ido-fallback-command)
  ;; also use up/down for navigating matches.
  (define-key ido-completion-map [down] 'ido-next-match) 
  (define-key ido-completion-map [up]   'ido-prev-match)
  ;; choose first match with TAB (like RETURN)
  (define-key ido-completion-map "\t"   'ido-exit-minibuffer)
  (define-key ido-completion-map (kbd "S-<tab>")   'ido-delete-backward-updir)
  (define-key ido-completion-map (kbd "C-h")   'ido-delete-backward-updir))

(add-hook 'ido-setup-hook #'ido-keybindings)

;; Enable ido-mode
(ido-mode t)

;; This allows partial matches, e.g. "tl" will match "Tyrion Lannister"
(setq ido-enable-flex-matching t)

;; Turn this behavior off because it's annoying
(setq ido-use-filename-at-point nil)
;; Tried its guessing algorithm below for a while. Not pleased.
;;(setq ido-use-filename-at-point 'guess)
(setq ido-use-url-at-point nil)

;; Don't try to match file across all "work" directories; only match files
;; in the current directory displayed in the minibuffer
(setq ido-auto-merge-work-directories-length -1)

;; Includes buffer names of recently open files, even if they're not open now
(setq ido-use-virtual-buffers t)

;; Don't ask permission for new buffers
(setq ido-create-new-buffer 'always)

;; Emphasize some file extensions over others
(setq ido-file-extensions-order '(".js" ".ts" ".scss" ".php" ".html" ".blade.php" ".emacs" ".el"))

;; This enables ido in all contexts where it could be useful, not just
;; for selecting buffer and file names
(ido-ubiquitous-mode t)
(ido-everywhere t)

;; Helm program model is incompatible with using TAB. Every couple months I try to
;; revisit Helm and end up disabling it in a minute and a half because of it.
;; ido-everywhere is incompatible with helm, don't forget to disable when enabling helm.
;; (when (require 'helm-config)
;;   (global-set-key (kbd "s-x") 'helm-M-x)
;;   (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
;;   (global-set-key (kbd "C-x C-f") #'helm-find-files)
;;   (helm-mode 1))

;; bind interactive regex search to C-M-r and C-M-s (add alt to search for regex)
;; swap regexp search and normal search
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)

; C-M-w copies to OS clipboard; C-M-y yanks from OS clipboard
(global-set-key "\C-\M-w" 'clipboard-kill-ring-save)
(global-set-key "\C-\M-y" 'clipboard-yank)

;; indented yank
(defun yank-and-indent ()
  "Yank and then indent the newly formed region according to mode."
  (interactive)
  (yank)
  (call-interactively 'indent-region))
;; by default, yank indented, but C-S-y will yank as-is
(global-set-key (kbd "C-y") 'yank-and-indent)
(global-set-key (kbd "C-S-y") 'yank)

;; by default, yank formatted
(global-set-key (kbd "C-1") 'yank-and-indent)
;; [reverted] [now using C-! as async-shell-command]
(global-set-key (kbd "C-!") 'yank)

(defun google (query)
  "Search google for QUERY."
  (interactive "sQuery: ")
  (browse-url (concat "http://www.google.com/search?q=" query)))

(global-set-key (kbd "M-?") '(lambda () (interactive) (google (symbol-name (symbol-at-point)))))




;;; SLIME

(setq inferior-lisp-program "sbcl")

;; WARNING: don't call (slime-setup ...) from inside 'slime-load-hook; will cause recursive load error
(slime-setup '(slime-fancy
               slime-repl-ansi-color
               slime-company
               slime-fuzzy
               slime-tramp))

(setq slime-docker-implementations `((sbcl ("sbcl") :docker-machine "default")))

(define-key slime-repl-mode-map (kbd "C-c ;")
  'slime-insert-balanced-comments)

;; (local-set-key [tab] ...) would change only the tab key and not C-i.
(define-key slime-repl-mode-map (kbd "<tab>") 'company-complete)
(define-key slime-repl-mode-map (kbd "C-c <tab>") 'slime-fuzzy-complete-symbol)

(define-key company-active-map (kbd "\C-n") 'company-select-next)
(define-key company-active-map (kbd "\C-p") 'company-select-previous)
(define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
(define-key company-active-map (kbd "M-.") 'company-show-location)

;; from https://stackoverflow.com/a/15808708/324220
(defun init-slime-configuration ()
  "Initialize slime configuration."
  ;; don't define keybindings here; neither with local-set-key nor with define-key.
  ;;
  (setq slime-load-failed-fasl 'never))


(add-hook 'slime-load-hook 'init-slime-configuration)

;; slime-mode useful keybindings:
;; C-d show documentation for company's current suggestion (e.g. a function)
;; M-. jumps to source of company's current suggestion.


;setq load-slime-by-hand t in .emacs on computers where you dont want auto slime
(when (and (boundp 'load-slime-by-hand) load-slime-by-hand)
  ;;if "cannot open load file, slime", then $ cd /usr/local && git clone https://github.com/slime/slime
  (add-to-list 'load-path "/usr/local/slime")
  (require 'slime)
  (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
  (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
  ;; Optionally, specify the lisp program you are using. Default is "lisp"
  (setq inferior-lisp-program "sbcl"))

;;; END SLIME



;;; plugins

;; cd ~/.emacs.d/ && git clone https://github.com/flowtype/flow-for-emacs.git
;;(load-file "~/.emacs.d/flow-for-emacs/flow.el")

; this is my little haskell plugin I'm writing to ease writing in Haskell
(load "~/dotemacs/haskellito/haskellito.el")


(defun enable-rainbow-delimiters ()
  ;; rainbow delimiters colors every delimiter pair with different color
  ;; with lame color theme, it's useless, but looks great nevertheless (actually found quite useful)
  (add-to-list 'load-path "~/.emacs.d/rainbow-delimiters/")
  ;;(global-rainbow-delimiters-mode) ; enable everywhere
  (when (require 'rainbow-delimiters nil 'noerror) 
    (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)))

;; control rainbow-delimiters vs paren-face (two incompatible paren decorators) with this variable
(setq color-parens-instead-of-dimming nil)

(if color-parens-instead-of-dimming
    (enable-rainbow-delimiters)
  ;; dims parentheses visually.
  (if (fboundp 'global-paren-face-mode)
      (global-paren-face-mode)))


;; (global-set-key (kbd "C-c g") 'magit-status)
;; (global-set-key [(super g)] 'magit-status)
;; (global-set-key (kbd "C-c M-g") 'magit-dispatch-popup)
;; (add-hook 'after-save-hook 'magit-after-save-refresh-status)

(defun diff-and-format-working-directory ()
  "Open a new buffer with nicely formatted git diff of current directory."
  (interactive)
  ;; this will open a window with the buffer containing diff output
  (shell-command "vcs-diff")
  ;; switch to that buffer
  (select-window (get-buffer-window "*Shell Command Output*"))
  (diff-mode))

(global-set-key (kbd "C-x g") 'diff-and-format-working-directory)


;causes massive inconveniences
;; (add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "C-M-i") 'company-complete);; C-M-i is the same as M-TAB
;; navigate suggestions popup using C-n/C-p
(define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
(define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)

;;(when (require 'web-mode nil 'noerror)
;;  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;;  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
;;  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
;;  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
;;  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;;  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
;;  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
;;  ;;
;;  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;;  (setq web-mode-engines-alist
;;      '(("php"    . "\\.phtml\\'")
;;        ("blade"  . "\\.blade\\."))))



(defun my-web-mode-indentation-hook ()
  "Indentation levels for web-mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4))
(add-hook 'web-mode-hook  'my-web-mode-indentation-hook)

(when (require 'web-mode-edit-element nil 'noerror)
  (add-hook 'web-mode-hook 'web-mode-edit-element-minor-mode))

;; try subword-mode in web-mode/html-mode
(add-hook 'html-mode-hook 'subword-mode)
(add-hook 'js-mode-hook 'subword-mode)
(add-hook 'web-mode-hook 'subword-mode)
(add-hook 'php-mode-hook 'subword-mode)

(defun html-mode-with-web-mode-helpers ()
  "I added some modifications to `html-mode` using `web-mode` functions (inaccessible from direct `html-mode`, so I first load web-mode (to load its functions) and then switch to `html-mode`)."
  (web-mode)
  (html-mode))

;; open .scss and .sass files in scss-mode
(add-to-list 'auto-mode-alist '("\\.blade.php\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.cshtml\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . html-mode-with-web-mode-helpers))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode));scss-mode or web-mode
(add-to-list 'auto-mode-alist '("\\.sass\\'" . scss-mode));scss-mode or web-mode
(add-to-list 'auto-mode-alist '("\\.vue\\'" . html-mode));html-mode or web-mode
;; ng2-ts-mode is automatically activated in *.component.ts and *.service.ts ng2.html-mode is automatically activated in *.component.html
;; no lookahead/lookbehind in elisp regex, so reset above .html binding
;; (add-to-list 'auto-mode-alist '("\\.ts\\'" . ng2-mode));web-mode
;;(add-to-list 'auto-mode-alist '("\\.component\\.html\\'" . ng2-html-mode))
;; ng2-html-mode garbles HTML tags on save  
(add-to-list 'auto-mode-alist '("\\.component\\.html\\'" . html-mode))
;; ###### WARNING: don't put extensions directly in the form of ".ext",..
;; ###### otherwise all other extension=>mode assignments will stop to work

;; good features but horribly slow
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; open React components in rjsx-mode. use magic-mode-alist to differentiate them from ordinary .js files.
;; depends on the file containing "import React.." by matching the file's contents to a multiline regex.
;; an alternative method would require prepending "// -*- mode: rjsx -*-" to each file, which would be unsustainable.
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*React" . rjsx-mode))

;; TODO es6 javascript mode - currently this Emacs installation has a bug and...
;; ... show packages like flycheck (required for this)
;; http://codewinds.com/blog/2015-04-02-emacs-flycheck-eslint-jsx.html

(require 'flycheck)

;; Disable the default flycheck jslint (use eslint instead)
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint json-jsonlist)))

(add-hook 'flycheck-mode-hook 'add-node-modules-path)

;; Enable eslint checker for jsx and javascript files
(flycheck-add-mode 'javascript-eslint 'rjsx-mode)
(flycheck-add-mode 'javascript-eslint 'javascript-mode)
;; Enable flycheck globally
;;27decdisableforperf;;(add-hook 'after-init-hook #'global-flycheck-mode)


;; to use web-mode instead of rjsx-mode for jsx files:
;; https://gist.github.com/CodyReichert/9dbc8bd2a104780b64891d8736682cea

;; begin new features

;; return to previous cursor position (by rotating mark-ring)
;; from http://stackoverflow.com/a/14539202/324220
;; tried (set-mark-command -1) but it returned to first recorded location, not last
(defun unpop-to-mark-command ()
  "Unpop off mark ring. Does nothing if mark ring is empty."
  (interactive)
      (when mark-ring
        (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
        (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
        (when (null (mark t)) (ding))
        (setq mark-ring (nbutlast mark-ring))
        (goto-char (marker-position (car (last mark-ring))))))
; feature - undo cursor position (return to previous position, something like pop-mark or pop-global-mark). maybe C-? (=C-S-/)
(global-set-key (kbd "C-?") 'unpop-to-mark-command)



(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'scss-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

(defun set-emmet-mode-settings ()
  ;; turn auto indent off (t for turning on)
  ;; (setq emmet-indent-after-insert nil)
  ;; if auto is off, then manual indent
  ;; (setq emmet-indentation 4)
  ;; positions the cursor between first empty quotes after expanding
  (setq emmet-move-cursor-between-quotes t) ;; default nil
  ;; to disable moving cursor after expanding
  ;; (setq emmet-move-cursor-after-expanding nil) ;; default t
  ;; will be useful in react-mode
  ;; (setq emmet-expand-jsx-className? t) ;; default nil
  ;; auto-closing tag format - <br />, <br/> or <br>
  (setq emmet-self-closing-tag-style " /") ;; default "/", can also be " /" and ""
  ;; jumping between emmet editing points
  (local-set-key (kbd "C-c <left>")  'emmet-prev-edit-point)
  (local-set-key (kbd "C-c <right>") 'emmet-next-edit-point)
  (local-set-key (kbd "C-S-<left>")  'emmet-prev-edit-point)
  (local-set-key (kbd "C-S-<right>") 'emmet-next-edit-point)
  (local-set-key (kbd "C-<")  'emmet-prev-edit-point)
  (local-set-key (kbd "C->") 'emmet-next-edit-point)
  ;; for home row keys
  (local-set-key (kbd "C-S-b")  'emmet-prev-edit-point)
  (local-set-key (kbd "C-S-f") 'emmet-next-edit-point)
  ;; allow navigating to edit points when using the georgian language input
  ;; TODO needs making C-ც the same as C-c
  ;;(local-set-key (kbd "C-ც <left>")  'emmet-prev-edit-point)
  ;;(local-set-key (kbd "C-ც <right>") 'emmet-next-edit-point)
  ;; set custom keybinding for expanding
  (local-set-key (kbd "C-c x") 'emmet-expand-line)
  ;; disable C-j expand keybinding (local-set-key didn't work)
  (define-key emmet-mode-keymap (kbd "C-j") nil))
  ;; use M-j instead
;;(define-key emmet-mode-keymap (kbd "M-j") 'emmet-expand-line))
(add-hook 'emmet-mode-hook 'set-emmet-mode-settings)

;; TODO find out how d/s/ inserts <div><span>...
;; TODO: differentiate between web-mode html, css and javascript
;; TODO: only call ..web-mode-html-.. in html minor mode and -js- in js minor mode
;; DOCS: http://web-mode.org/
(add-hook 'web-mode-hook 'add-web-mode-html-bindings)
(add-hook 'html-mode-hook 'add-web-mode-html-bindings)
(add-hook 'ng2-html-mode-hook 'add-web-mode-html-bindings)

(defun add-web-mode-html-bindings ()
  ;; with an annoying popup, flycheck reports too many errors in HTML files.
  (flycheck-mode -1)
  ;; insert new tag
  (local-set-key (kbd "C-c i") 'web-mode-element-insert)
  ;; go back one tag
  (local-set-key (kbd "C-c C-b") 'sgml-skip-tag-backward)
  (local-set-key (kbd "M-p") 'sgml-skip-tag-backward)
  ;; go forward one tag
  (local-set-key (kbd "C-c C-f") 'sgml-skip-tag-forward)
  ;; (local-set-key (kbd "M-n") 'sgml-skip-tag-forward)
  ;; see OLD:/NEW: comment above
  ;; ALSO: move to M-S-n -- 'make-frame isn't used often, and M-n/M-p are too useful to waste.
  (local-set-key (kbd "M-S-n") 'make-frame)
  ;; enter (go inside) tag
  (local-set-key (kbd "C-c e") 'web-mode-dom-traverse)
  ;; exit (go outside) current tag
  (local-set-key (kbd "C-c u") 'web-mode-element-parent)
  (local-set-key (kbd "C-c C-u") 'web-mode-element-parent)
  ;; transpose tags before and after cursor
  (local-set-key (kbd "C-c t") 'sgml-transpose-tags-around-cursor)
  ;; delete tags before or after the cursor
  (local-set-key (kbd "C-c DEL") 'sgml-delete-tag-backward)
  (local-set-key (kbd "C-c h") 'sgml-delete-tag-backward)
  (local-set-key (kbd "C-c d") 'sgml-delete-tag-forward)
  ;; close tag (possible addition: prepend newline&indent when start tag is not on the same line and we're closing tag on a non-empty line)
  (local-set-key (kbd "C-c /") 'sgml-close-tag)
  ;; select tag before cursor
  (local-set-key (kbd "C-c b") 'sgml-select-tag-backward)
  ;; select tag after cursor
  (local-set-key (kbd "C-c f") 'sgml-select-tag-forward)
  ;; duplicate (at the cursor position) the tag before cursor
  (local-set-key (kbd "C-c C-v") 'sgml-duplicate-previous-tag)
  ;; duplicate (at the cursor position) the tag after cursor
  (local-set-key (kbd "C-c C-o") 'sgml-duplicate-next-tag)
  ;; wrap selection/tag in a new parent tag
  (local-set-key (kbd "C-c w") 'web-mode-element-wrap)
  (local-set-key (kbd "C-c C-p") 'preview-current-buffer-on-localhost)
  (local-set-key (kbd "C-c C-S-p") 'preview-current-buffer-on-crmfrontend)
  (local-set-key (kbd "C-c c") 'sgml-clean-tag-after-cursor)
  (local-set-key (kbd "C-c k") 'sgml-kill-tag-contents-after-cursor))

(global-set-key (kbd "C-S-c") 'inspect-element)

(defun add-web-mode-js-bindings ()
  (local-set-key (kbd "C-c f") 'open-js-lambda-block))


;; (defun symbol-before-cursor ()
;;   "Returns the symbol (word with optional dashes) before the cursor from current buffer"
;;   (let ((cursor-at (point)))
;;     (save-excursion
;;       (backward-sexp 1)
;;       (buffer-substring (point) cursor-at))))



(load "~/dotemacs/css-autocomplete.el")

(add-hook 'scss-mode-hook
          (lambda ()
                  (local-set-key (kbd ":") 'autocomplete-css-property)
                  (local-set-key (kbd ";") (lambda ()
                                                   (interactive)
                                                   (autocomplete-css-value t)))
                  (local-set-key (kbd "C-;") (lambda ()
                                                     (interactive)
                                                     (autocomplete-css-value nil)))))


;; end CSS autocomplete


;;; BEGIN ESC keybindings (quick to use, intended to replace longer C-x keystrokes)
;;; also esc-arrows for navigating between split windows is located in #'set-windmove-keybindings
;;(global-set-key (kbd "<escape> k") 'kill-buffer)
;; I prefer the immediacy of kill-current-buffer, without confirmation.
(global-set-key (kbd "<escape> k") 'kill-current-buffer)
(global-set-key (kbd "<escape> n") 'make-frame)
(global-set-key (kbd "<escape> 0") 'delete-window)
(global-set-key (kbd "<escape> <escape> 0") 'delete-frame)
(global-set-key (kbd "<escape> 5") 'delete-frame)
(global-set-key (kbd "<escape> q") 'ask-before-closing)
(global-set-key (kbd "<escape> e") 'eval-last-sexp)
(global-set-key (kbd "<escape> f") 'find-file)
(global-set-key (kbd "<escape> w") 'write-file)
(global-set-key (kbd "<escape> s") 'save-buffer)
(global-set-key (kbd "<escape> b") 'switch-to-buffer)
(global-set-key (kbd "<escape> l") 'buffer-menu)
(global-set-key (kbd "<escape> <escape> b") 'buffer-menu)
(global-set-key (kbd "<escape> o") 'other-window)
(global-set-key (kbd "<escape> y") 'yank-and-indent)
(global-set-key (kbd "<escape> DEL") 'backward-kill-word)
;;;  bind ESC-g to triple ESC, otherwise doesn't behave like C-g
(global-set-key (kbd "<escape> g") 'keyboard-escape-quit)
(global-set-key (kbd "<escape> x") 'execute-extended-command)
(global-set-key (kbd "<escape> 1") 'shell-command)
(global-set-key (kbd "<escape> 2") 'split-and-switch-window-below)
(global-set-key (kbd "<escape> 3") 'split-and-switch-window-right)
(global-set-key (kbd "<escape> /") 'undo)
;;; END ESC keybindings


;; function camelify(snake){
;;     return snake.split('-').map((part, i) => {
;;         if(i==0){ return part; } else { return part.substr(0,1).toUpperCase() + part.substr(1) }
;;     }).join('');
;; }
;; begin custom keyboard macros
;; converts one line of .selector { pro-perty: val; } to doc.query(".selector").style.proPerty = "val";
(fset 'css-to-js
   [?\C-e ?\C-s ?. ?\C-m ?\C-b ?d ?o ?c ?u ?m ?e ?n ?t ?. ?q ?u ?e ?r ?y ?S ?e ?l ?e ?c ?t ?o ?r ?A ?l ?l ?\( ?\" ?\C-s ?\{ ?\C-m ?\C-? ?\M-  ?\M-  ?\M-  ?\C-? ?\" ?\) ?. ?f ?o ?r ?E ?a ?c ?h ?\( ?e ?l ?  ?= ?> ?\S-  ?e ?l ?. ?s ?t ?y ?l ?e ?\[ ?c ?a ?m ?e ?l ?i ?f ?y ?\( ?\" ?\M-  ?\C-? ?\C-s ?: ?\C-m ?\C-? ?\" ?\) ?\] ?\M-  ?\C-? ?  ?= ?  ?\M-  ?\" ?\C-s ?\; ?\C-m ?\C-b ?\" ?\C-s ?\} ?\C-m ?\C-? ?\M-  ?\C-? ?\C-? ?\) ?\;])
;; end custom keyboard macros



(defun preview-file-on-localhost (file)
  (shell-command (concat "open http://localhost:3000/" file)))

(defun preview-file-on-crmfrontend (file)
  (shell-command (concat "open http://crmfrontend-dev.lb.ge/" file)))

(defun current-buffer-filename ()
  (file-name-nondirectory (buffer-file-name)))

(defun preview-current-buffer-on-localhost ()
  (interactive)
  (preview-file-on-localhost (current-buffer-filename)))

(defun preview-current-buffer-on-crmfrontend ()
  (interactive)
  (preview-file-on-crmfrontend (current-buffer-filename)))

(defun switch-to-browser ()
  (interactive)
  ;; we can ask the OS config too; see AppleScript Inspect script in iCloud
  (shell-command "open /Applications/Google\\ Chrome.app"))

;; /icloud and ~/icloud are symlinks to /Users/luka/Library/Mobile documents/
;; /icloud/scripts is a symlink to "com~apple~scripteditor2/documents" in the /icloud directory

;; (do-applescript script) only accepts applescript code, not a path to the file

(defun inspect-element ()
  (interactive)
  ;; todo we could detect a buffer with html template and do a c-c c-p before inspecting (custom html previewing fn)
  (shell-command "osascript /icloud/scripts/inspect.scpt"))

(setq project-configs
      '((lb . ((dir . "/projects/lb")
               (serve-cmd . "gulp serve")))
        (bk . ((dir . "/projects/bookulus")
               (serve-cmd . "npm run dev")))
        (meo-phaser . ((dir . "/projects/meomari")
                       (serve-cmd . "npm start")
                       (left-window-file . "/src/index.js")
                       (right-window-file . "/src/assets")))
        (meo . ((dir . "/projects/meo")
                (serve-cmd . "npm start")
                (left-window-file . "/src")
                (right-window-file . "/src/App.js")))
        (rx . ((dir . "/projects/timers-app")
                (serve-cmd . "npm run dev")
                (left-window-file . "/pages")
                (right-window-file . "/pages/index.js")))
        (cr . ((dir . "/projects/carrent")
               (serve-cmd . "npm run watch")
               (left-window-file . "/app")
               (right-window-file . "/app/Http/Controllers")))
        (bl . ((dir . "/projects/bloom")
               (serve-cmd . "npm run dev")
               (left-window-file . "/scss")
               (right-window-file . "/views")))))

;; project-mode
(defun project-mode (project-name)
  (let* ((serve-buffer-name (concat (symbol-name project-name) "-serve"))
         (project-config (cdr (assoc project-name project-configs)))
         (project-dir (cdr (assoc 'dir project-config)))
         ;; latest .emacs config converted to eshell, so run bash directly
         (serve-cmd (concat "/bin/bash -c \"" (cdr (assoc 'serve-cmd project-config)) "\""))
         (left-window-file (or (cdr (assoc 'left-window-file project-config)) "/scss"))
         (right-window-file (or (cdr (assoc 'right-window-file project-config)) "/views"))
         (show-serve-window-p nil))
    (if (get-buffer "*open recent*")
        (kill-buffer "*open recent*"))
    (if (get-buffer "*messages*")
        (kill-buffer "*messages*"))
    ;; if project-mode has already been called and gulp/serve cmd is running, then stay in current buffer
    (unless (get-buffer serve-buffer-name)
      (switch-to-buffer "*scratch*"))
    (cd project-dir)
    ;; (clean-buffer-list) can also be used if it's a long session (closes unused, unmodified buffers)
    ;; close all windows to avoid nested calls
    (delete-other-windows)
    ;; this will focus on the right window
    (split-and-switch-window-right)
    ;; open terminal (auto-reload server) in the right window
    ;; if it's already running, just switch to it
    (if (get-buffer serve-buffer-name)
        (switch-to-buffer serve-buffer-name)
      (progn
        (eshell "new")
        (rename-buffer serve-buffer-name)
        ;;(eshell/exec "source ~/.bashrc");doesn't work
        (eshell/exec serve-cmd)))
    ;; this will open a bottom window (by default, *open recent*) focus on it
    (split-and-switch-window-below)
    (if show-serve-window-p
        (progn
          ;; enlarge bottom window (and shrink the top terminal window)
          (enlarge-window 20))
      (progn
        ;; instead, hide the gulp serve - not productive
        (windmove-up)
        (delete-window)))
    ;; bring focus to the left window
    (windmove-left)
    ;; open find file dialog
    ;;(call-interactively 'find-file)
    ;; open scss directory in the left tab
    (switch-to-buffer (find-file (concat project-dir left-window-file)))
    (windmove-right)
    ;; open views directory in the right tab
    (switch-to-buffer (find-file (concat project-dir right-window-file)))
    (split-and-switch-window-below)
    (delete-window)
    ;; bring focus back to the left window
    (windmove-left)))

(defun lb-mode ()  "Project lb workspace." (interactive) (project-mode 'lb))
(defun meo-mode () "Project meo workspace." (interactive) (project-mode 'meo))
(defun rx-mode () "Project rx workspace." (interactive) (project-mode 'rx))
(defun cr-mode () "Project cr workspace." (interactive) (project-mode 'cr))
(defun bl-mode () "Project bl workspace." (interactive) (project-mode 'bl))






;;; try paredit for a while.
;;; had to disable almost immediately. Gives `unbalanced parentheses` error on correct parens.

;; ;; Automatically load paredit when editing a lisp file
;; ;; More at http://www.emacswiki.org/emacs/ParEdit
;; (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
;; (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
;; (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
;; (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
;; (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
;; (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
;; (add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; eldoc-mode shows documentation in the minibuffer when writing code
;; http://www.emacswiki.org/emacs/ElDoc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)


;;;; Clojure mode with cider
;; for performance, move to fn; don't load by default
(defun setup-clojure ()
  (interactive)
  "Setup and configure cloju packages, cider, etc."
  (load "~/dotemacs/setup-clojure.el"))

(defun clojure-repl ()
  (interactive)
  (cider-jack-in))



(provide '.emacs)
;;; .emacs ends here
