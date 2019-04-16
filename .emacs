;;;;
;;;; Luka Ramishvili's .emacs file
;;;;

;;; use C-x C-e to reload any s-exp

;;; functions

(defun bool (arg)
  (not (not arg)))

(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
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
  (insert command)
  (eshell-send-input))

(defun eshell/x ()
  (insert "exit")
  (eshell-send-input)
  (delete-window))

(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed"
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to exit Emacs? "))
      (if (< emacs-major-version 22)
          (save-buffers-kill-terminal)
        (save-buffers-kill-emacs))
    (message "Canceled exit")))

;; maximize emacs frame on startup (X11-specific but I'm not using anything else)
(defun x11-maximize-frame ()
  "Maximize the current frame (to full screen)"
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0)))
;;(x11-maximize-frame)

(defun insert-double-quotes (&optional arg)
  "Inserts double quotes and places the cursor between them"
  (interactive "P")
  (insert-pair arg ?\" ?\"))
 
  ;; (insert "\"")
  ;; (save-excursion
  ;;   (insert "\"")))


;; previous-mode is already defined. other-window will throw error if you redefine this
(defun switch-to-previous-window ()
  (interactive)
  (other-window -1))

(defun inside-string? ()
  "Returns non-nil if inside string, else nil.
This depends on major mode having setup syntax table properly."
  (interactive)
  (let ((result (nth 3 (syntax-ppss))))
    (message "%s" result)
    result))

(fset 'original-backward-up-list (symbol-function 'backward-up-list))
(defun backward-up-list ()
  (interactive)
  (cond
   ((equal (inside-string?) 34)
    (search-backward "\"")); get out of "" string
   ((equal (inside-string?) 39)
    (search-backward "'")))
  (original-backward-up-list))

;; variations on Steve Yegge recommendations
(defun kill-current-word ()
  (interactive)
  ;;in case cursor was at the end of current word, prevent jumping to next word's end
  (left-word 1)
  (right-word 1)
  (backward-kill-word 1))
(defun kill-current-symbol ()
  (interactive)
  (backward-sexp 1)
  (kill-sexp 1))
(defun kill-current-line ()
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
  "If at end of line, join with following; otherwise kill line.
Deletes whitespace at join."
  (interactive "P")
  (kill-line arg)
  (hungry-delete-forward 0))
;; this is a reverse of C-k (deletes line contents *before* cursor)
(defun backward-kill-line ()
  (interactive)
  (set-mark-command nil)
  (move-beginning-of-line 1)
  (backward-delete-char-untabify 1))

(defun whitespacep (c)
  (bool
   (cond ((characterp c) (or (char-equal c #x9)
                             (char-equal c #xa)
                             (char-equal c #x20)))
         ((stringp c) (or (equal c "\t")
                          (equal c "\n")
                          (equal c " ")))
         (t nil))))

(defun kill-whitespace-around-cursor ()
  (interactive)
  (if (whitespacep (preceding-char))
      (hungry-delete-backward 0))
  (if (whitespacep (following-char))
      (hungry-delete-forward 0)))

;; kills all whitespace around cursor and leaves only one space
(defun just-one-whitespace ()
  (interactive)
  (kill-whitespace-around-cursor)
  (insert " "))

(defun kill-whitespace-around-line ()
  (interactive)
  (move-beginning-of-line 1)
  (kill-whitespace-around-cursor)
  (just-one-space)
  (move-end-of-line 1)
  (kill-whitespace-around-cursor)
  (just-one-space))

(defun add-whitespace-around-line ()
  (interactive)
  (move-beginning-of-line 1)
  (newline-and-indent)
  (move-end-of-line 1)
  (newline-and-indent))

;; M-S-space: useful for just formatting a single-line block with no intention of adding code
(defun add-whitespace-around-block (&optional add-extra-line-p)
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
  (interactive)
  (add-whitespace-around-block t))

(defun comment-line ()
  (interactive)
  (move-beginning-of-line 1)
  (set-mark-command nil)
  (move-end-of-line 1)
  ;;will call comment-region
  (comment-dwim nil))

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
;; inserts anonymous function at cursor
(defun open-js-lambda-block ()
  (interactive)
  (insert "function()")
  (open-brackets-block nil))

(defun sgml-transpose-tags-around-cursor ()
  ;; transpose tags before and after cursor
  (interactive)
  ;; transpose only works when cursor is at first element's beginning
  (sgml-skip-tag-backward 1)
  (web-mode-element-transpose))
(defun sgml-delete-tag-backward (arg)
  ;; delete the tag before cursor
  (interactive "p")
  (let ((tag-start (point)))
    (sgml-skip-tag-backward arg)
    (kill-region tag-start (point))))
(defun sgml-delete-tag-forward (arg)
  ;; delete the tag before cursor
  (interactive "p")
  (let ((tag-end (point)))
    (sgml-skip-tag-forward arg)
    (kill-region tag-end (point))))
(defun sgml-select-tag-backward (arg)
  ;; select tag before cursor (includes any space between current cursor position and closing tag
  (interactive "p")
  (set-mark-command nil)
  (sgml-skip-tag-backward arg))
(defun sgml-select-tag-forward (arg)
  ;; select tag before cursor (includes any space between current cursor position and opening tag
  (interactive "p")
  (set-mark-command nil)
  (sgml-skip-tag-forward arg))
(defun sgml-duplicate-previous-tag (arg)
  ;; insert the contents of the tag before cursor at the current cursor position
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
  ;; insert the contents of the tag after cursor at the current cursor position
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
(defmacro sgml-with-tag-contents-after-cursor (&rest op)
  `(progn
     ;; TODO: we need to somehow avoid matching closing tags, which also contain ">"'s.
     ;; if we simply search for ">", this code will fail when the cursor is inside a closing tag or before a tag with children (e.g. </di|v> or <di|v> <div>contents</div> </div>, respectively)
     (search-forward ">")
     (set-mark-command nil)
     (search-forward "<")
     (backward-char 1)
     ,@op))
(defun sgml-clean-tag-after-cursor ()
  (interactive)
  (sgml-with-tag-contents-after-cursor
   (delete-region (region-beginning) (region-end))))
(defun sgml-kill-tag-contents-after-cursor ()
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

;;; from better-defaults
(global-set-key (kbd "s-SPC") 'hippie-expand)

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

;; display project1/samename.js instead of samename.js<project1>
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; a better buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-x C-g") 'goto-line)

(show-paren-mode 1)
(setq-default indent-tabs-mode nil)
(setq save-interprogram-paste-before-kill t
      apropos-do-all t
      ;;mouse-yank-at-point t
      require-final-newline t
      visible-bell t
      load-prefer-newer t
      ediff-window-setup-function 'ediff-setup-windows-plain
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))

;; disabled guru mode; doesn't play well with multiple input sources (need arrows with non-english layouts)
;; (add-to-list 'load-path "~/.emacs.d/guru-mode")
;; (require 'guru-mode)
;; (guru-global-mode +1)


;;; colors
;;(set-background-color "#3f3f3f")
;;(set-foreground-color "white")

;; themes
(add-to-list 'custom-theme-load-path "~/dotemacs/blackboard-theme")
(load-theme 'blackboard t)

;;; fonts
(set-default-font "DejaVu Sans Mono")

(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

(cd "/projects/")

(setq default-directory "/projects/")

;; disable tab indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; enable C-x C-u and C-x C-l (for upcasing/downcasing selection/region)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; always follow symlinks (avoid annoying yes/no question)
(setq vc-follow-symlinks t)

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

;; Find file in current directory:
(global-set-key (kbd "C-M-,") 'find-file-in-current-directory)

(when window-system
  (global-set-key (kbd "C-x C-c") 'ask-before-closing)
  (global-set-key (kbd "s-q") 'ask-before-closing))

;; stops selection with a mouse being immediately injected to the kill ring
(setq mouse-drag-copy-region nil)
;; hide the toolbar (check if available, or signals error in terminal)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;; hide the menu (no benefits in hiding the menu on osx)
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
;; hide the scrollbars, not using them anyway
;; also check if available, or signals error in terminal
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;; show column numbers
(column-number-mode)

(defun insert-square-brackets (&optional arg)
  (interactive "P")
  (insert-pair arg ?\[ ?\]))

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

(setq mac-command-modifier 'control)
(setq mac-control-modifier 'super)
;; there's also 'control (C-), 'meta (M-), 'super (S-) and 'hyper (H-)

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

(global-set-key (kbd "<C-S-backspace>") 'backward-kill-line)
(global-set-key (kbd "C-M-h") 'backward-kill-sexp)
(global-set-key (kbd "<C-M-backspace>") 'backward-kill-sexp)

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
(global-set-key [(super h)] 'help-command)

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
         diredful
         ;;; optional packages
         php-mode
         slime
         sclang-extensions
         flymd
         markdown-mode
         markdown-preview-mode
         adoc-mode
         ;;; w3m needed for SuperCollider help system
         w3m))
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

;;show free keybindings on s-h s-k
(require 'free-keys)
(global-set-key (kbd "s-h s-k") 'free-keys)

(require 'diredful)
(diredful-mode 1)

;;(when (require 'helm-config)
;;  (global-set-key (kbd "s-x") 'helm-M-x)
;;  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
;;  ;;(global-set-key (kbd "C-x C-f") #'helm-find-files)
;;  (helm-mode 1))


(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))


(require 'hungry-delete)

;;(use-package hungry-delete
;;             :bind (("<backspace>" . hungry-delete-backward)
;;                    ("C-S-d" . hungry-delete-backward)
;;                    ("C-h" . hungry-delete-backward)
;;                    ("C-d" . hungry-delete-forwa


;;; Asciidoc. don't forget `brew install asciidoc`. Docs: https://asciidoctor.org/docs/user-manual/
;;; To convert to HTML5 (default asciidoc output), use `asciidoc file.adoc` (will output file.html)
;;; a2x (Asciidoc to *) behaved weirdly, ran xmllint which detected a2x's own errors. `a2x a2x --format=xhtml [same as -f] --no-xmllint [same as -L]`
;;; pandoc has no Asciidoc reader (so cannot convert *from* Asciidoc).


;;; Markdown

(require 'markdown-mode)
(require 'markdown-preview-mode)

;; open flymd's live-reload in Firefox
(defun my-flymd-browser-function (url)
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
  ;; each call to preview creates new websocket server, so don't auto-preview
  ;; (markdown-preview-function)
  (local-set-key (kbd "C-c C-p") (lambda ()
                                     (interactive)
                                     (markdown-preview-function))))




(global-set-key (kbd "C-M-\\") 'kill-whitespace-around-cursor)
(global-set-key (kbd "<C-M-SPC>") 'just-one-whitespace)

(global-set-key (kbd "C-M-|") 'kill-whitespace-around-line)

(global-set-key (kbd "C-M-S-SPC") 'add-whitespace-around-line)
(global-set-key (kbd "M-S-SPC") 'add-whitespace-around-block)
(global-set-key (kbd "<M-S-return>") 'add-whitespace-around-block-and-newline)

(global-set-key (kbd "C-M-;") 'comment-line)


(global-set-key (kbd "C-z") 'undo)
;; these keys are close to and frequently mistyped as the undo sequence, C-/
(global-set-key (kbd "C-.") 'undo)
(global-set-key (kbd "C-,") 'undo)
(global-set-key (kbd "C--") 'undo)

;; php mode keybindings
(add-hook 'php-mode-hook 'my-php-mode-stuff)

(defun my-php-mode-stuff ()
  (local-set-key (kbd "<f1>") 'my-php-function-lookup)
  (local-set-key (kbd "C-<f1>") 'my-php-symbol-lookup)
  (local-set-key (kbd "C-M-h") 'backward-kill-word))

(defun my-php-symbol-lookup ()
  (interactive)
  (let ((symbol (symbol-at-point)))
    (if (not symbol)
        (message "No symbol at point.")
      
      (browse-url (concat "http://php.net/manual-lookup.php?pattern="
                          (symbol-name symbol))))))

(defun my-php-function-lookup ()
  (interactive)
  (let* ((function (symbol-name (or (symbol-at-point)
                                    (error "No function at point."))))
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

(add-hook 'js-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c f") 'open-js-lambda-block)))

(defun set-windmove-keybindings ()
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

  (global-set-key (kbd "M-n") 'new-frame)

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
  (interactive)
  (recentf-mode)
  (recentf-open-files))
;; only useful without project-mode; but when enabled, messes up project-mode.
;; on load, show recent file list
;;(add-hook 'window-setup-hook 'show-recent-file-list)
(defun on-new-window ()
  (other-window 1)
  (show-recent-file-list))
; when splitting a window ("tab"), show recent file list
(defun split-and-switch-window-below ()
  (interactive)
  (split-window-below)
  (on-new-window))
(defun split-and-switch-window-right ()
  (interactive)
  (split-window-right)
  (on-new-window))
(global-set-key (kbd "C-x 2") 'split-and-switch-window-below)
(global-set-key (kbd "C-x 3") 'split-and-switch-window-right)
; quick shortcuts for invoking recent file list
(global-set-key (kbd "C-x M-f") 'show-recent-file-list)
;; was causing error when installing slime - "Key sequence C-x C-a C-l starts with non-prefix key C-x C-a"
;; (global-set-key (kbd "C-x C-a") 'show-recent-file-list)

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
(global-set-key (kbd "C-!") 'yank)

(defun google (query)
  "googles a query"
  (interactive "sQuery: ")
  (browse-url (concat "http://www.google.com/search?q=" query)))

(global-set-key (kbd "M-?") '(lambda () (interactive) (google (symbol-name (symbol-at-point)))))


; from Tikhon Jelvis (modified to include bash profile)
(defun new-shell (name)
  "Opens a new shell buffer with the given name in
asterisks (*name*) in the current directory and changes the
prompt to 'name>'."
  (interactive "sName: ")
  (pop-to-buffer (concat "*" name "*"))
  (unless (eq major-mode 'shell-mode)
    (shell (current-buffer))
    (sleep-for 0 200)
    (delete-region (point-min) (point-max))
    ;; set prompt name && include user bash profile
    (comint-simple-send (get-buffer-process (current-buffer)) 
                        (concat "export PS1=\"\033[33m" name "\033[0m:\033[35m\\W\033[0m>\" && source ~/.bash_profile"))))
(global-set-key "\C-c\ s" 'new-shell)



(setq inferior-lisp-program "sbcl")

;; from https://stackoverflow.com/a/15808708/324220
(defun init-slime-configuration ()
  ;; causes recursive load error
  ;;(slime-setup '(slime-fancy slime-fuzzy))
  (slime-setup)
  (setq slime-load-failed-fasl 'never)
  ;;causes symbol definition is void error
  ;;(define-key slime-repl-mode-map (kbd "<tab>") 'slime-fuzzy-complete-symbol)
  (define-key slime-mode-map (kbd "<tab>") 'slime-fuzzy-complete-symbol))

(add-hook 'slime-load-hook 'init-slime-configuration)


;setq load-slime-by-hand t in .emacs on computers where you dont want auto slime
(when (and (boundp 'load-slime-by-hand) load-slime-by-hand)
  ;;if "cannot open load file, slime", then $ cd /usr/local && git clone https://github.com/slime/slime
  (add-to-list 'load-path "/usr/local/slime")
  (require 'slime)
  (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
  (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
  ;; Optionally, specify the lisp program you are using. Default is "lisp"
  (setq inferior-lisp-program "sbcl"))


;;; plugins

; this is my little haskell plugin I'm writing to ease writing in Haskell
(load "~/dotemacs/haskellito/haskellito.el")

;; rainbow delimiters colors every delimiter pair with different color
;; with lame color theme, it's useless, but looks great nevertheless
(add-to-list 'load-path "~/.emacs.d/rainbow-delimiters/")
(when (require 'rainbow-delimiters nil 'noerror) 
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode))
;;(global-rainbow-delimiters-mode) ; enable everywhere




;; (global-set-key (kbd "C-c g") 'magit-status)
;; (global-set-key [(super g)] 'magit-status)
;; (global-set-key (kbd "C-c M-g") 'magit-dispatch-popup)
;; (add-hook 'after-save-hook 'magit-after-save-refresh-status)

(defun diff-and-format-working-directory ()
  (interactive)
  ;; this will open a window with the buffer containing diff output
  (shell-command "vcs-diff")
  ;; switch to that buffer
  (select-window (get-buffer-window "*Shell Command Output*"))
  (diff-mode))

(global-set-key (kbd "C-x g") 'diff-and-format-working-directory)


;causes massive inconveniences
;(add-hook 'after-init-hook 'global-company-mode)
;(global-set-key (kbd "S-SPC") 'company-complete)


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



;; I added some modifications to html-mode using web-mode functions (inaccessible from direct html-mode, so I first load web-mode (to load its functions) and then switch to html-mode)
(defun html-mode-with-web-mode-helpers ()
  (web-mode)
  (html-mode))

;; open .scss and .sass files in scss-mode
(add-to-list 'auto-mode-alist '("\\.blade.php\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.cshtml\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . html-mode-with-web-mode-helpers))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode));scss-mode or web-mode
(add-to-list 'auto-mode-alist '("\\.sass\\'" . scss-mode));scss-mode or web-mode
(add-to-list 'auto-mode-alist '("\\.vue\\'" . html-mode));html-mode or web-mode
(add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
;; ###### WARNING: don't put extensions directly in the form of ".ext",..
;; ###### otherwise all other extension=>mode assignments will stop to work

;; good features but horribly slow
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; TODO es6 javascript mode - currently this Emacs installation has a bug and...
;; ... show packages like flycheck (required for this)
;; http://codewinds.com/blog/2015-04-02-emacs-flycheck-eslint-jsx.html


;; disable web-mode from every file. other ways didn't work. very annoying.
;; DIDN'T WORK
;;(rassq-delete-all 'web-mode auto-mode-alist)


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
  (define-key emmet-mode-keymap (kbd "C-j") nil)
  ;; use M-j instead
  (define-key emmet-mode-keymap (kbd "M-j") 'emmet-expand-line))
(add-hook 'emmet-mode-hook 'set-emmet-mode-settings)

;; TODO find out how d/s/ inserts <div><span>...
;; TODO: differentiate between web-mode html, css and javascript
;; TODO: only call ..web-mode-html-.. in html minor mode and -js- in js minor mode
;; DOCS: http://web-mode.org/
(add-hook 'web-mode-hook 'add-web-mode-html-bindings)
(add-hook 'html-mode-hook 'add-web-mode-html-bindings)

(defun add-web-mode-html-bindings ()
  ;; insert new tag
  (local-set-key (kbd "C-c i") 'web-mode-element-insert)
  ;; go back one tag
  (local-set-key (kbd "C-c C-b") 'sgml-skip-tag-backward)
  (local-set-key (kbd "M-p") 'sgml-skip-tag-backward)
  ;; go forward one tag
  (local-set-key (kbd "C-c C-f") 'sgml-skip-tag-forward)
  ;; (local-set-key (kbd "M-n") 'sgml-skip-tag-forward)
  ;; see OLD:/NEW: comment above
  (local-set-key (kbd "M-n") 'new-frame)
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


(defun symbol-before-cursor ()
  "Returns the symbol (word with optional dashes) before the cursor from current buffer"
  (let ((cursor-at (point)))
    (save-excursion
      (backward-sexp 1)
      (buffer-substring (point) cursor-at))))

(defun insert-semicolon-consider-existing (&optional append-space)
  "If there's a semicolon after cursor, jump through it instead of adding another"
  (interactive)
  (if (equal ";" (buffer-substring (point) (+ (point) 1)))
      (forward-char 1)
    (insert ";"))
  (when append-space
    (if (equal " " (buffer-substring (point) (+ (point) 1)))
        (forward-char 1)
      (insert " "))))

(defun autocomplete-css-property ()
  "When user types keyword followed by a colon, autocomplete from predetermined list"
  (interactive)
  (let* ((inserters '((a-c . "align-content")
                      (a-i . "align-items")
                      (a-s . "align-self")
                      (bg . "background")
                      (b-c . "background-color")
                      (b-i . "background-image")
                      (b-p . "background-position")
                      (b-s . "background-size")
                      (bo . "border")
                      (bor . "border")
                      (bo-c . "border-color")
                      (bor-c . "border-color")
                      (bo-s . "border-style")
                      (bor-s . "border-style")
                      (box-s . "box-shadow")
                      (bw . "border-width")
                      (b-w . "border-width")
                      (bo-w . "border-width")
                      (bor-w . "border-width")
                      (b-t . "border-top")
                      (b-t-w . "border-top-width")
                      (b-t-s . "border-top-style")
                      (b-t-c . "border-top-color")
                      (b-r . "border-right")
                      (b-r-w . "border-right-width")
                      (b-r-s . "border-right-style")
                      (b-r-c . "border-right-color")
                      (b-b . "border-bottom")
                      (b-b-w . "border-bottom-width")
                      (b-b-s . "border-bottom-style")
                      (b-b-c . "border-bottom-color")
                      (b-l . "border-left")
                      (b-l-w . "border-left-width")
                      (b-l-s . "border-left-style")
                      (b-l-c . "border-left-color")
                      (bo-r . "border-radius")
                      (bor-r . "border-radius")
                      (b-t-l-r . "border-top-left-radius")
                      (b-t-r-r . "border-top-right-radius")
                      (b-b-l-r . "border-bottom-left-radius")
                      (b-b-r-r . "border-bottom-right-radius")
                      (b . "bottom")
                      (c . "color")
                      (co . "content")
                      (cu . "cursor")
                      (cur . "cursor")
                      (d . "display")
                      (f . "flex")
                      (f-d . "flex-direction")
                      (f-g . "flex-grow")
                      (fl . "float")
                      (f-f . "font-family")
                      (f-s . "flex-shrink")
                      (fz . "font-size")
                      (f-st . "font-style")
                      (f-w . "flex-wrap")
                      (fw . "font-weight")
                      (h . "height")
                      (j-c . "justify-content")
                      (jc . "justify-content")
                      (l . "left")
                      (l-s . "letter-spacing")
                      (l-s-t . "list-style-type")
                      (lst . "list-style-type")
                      (l-h . "line-height")
                      (lh . "line-height")
                      (m . "margin")
                      (m-t . "margin-top")
                      (mt . "margin-top")
                      (m-r . "margin-right")
                      (mr . "margin-right")
                      (m-b . "margin-bottom")
                      (mb . "margin-bottom")
                      (m-l . "margin-left")
                      (ml . "margin-left")
                      (min-w . "min-width")
                      (min-h . "min-height")
                      (max-w . "max-width")
                      (max-h . "max-height")
                      (o . "order")
                      (or . "order")
                      (op . "opacity")
                      (ov . "overflow")
                      (o-x . "overflow-x")
                      (o-y . "overflow-y")
                      (ov-x . "overflow-x")
                      (ov-y . "overflow-y")
                      (p . "padding")
                      (p-t . "padding-top")
                      (p-r . "padding-right")
                      (p-b . "padding-bottom")
                      (p-l . "padding-left")
                      (p-e . "pointer-events")
                      (po-e . "pointer-events")
                      (po . "position")
                      (r . "right")
                      (t-a . "text-align")
                      (t-d . "text-decoration")
                      (to . "text-overflow")
                      (t-o . "transform-origin")
                      (t-s . "text-shadow")
                      (t-i . "text-indent")
                      (t-t . "text-transform")
                      (t . "top")
                      (tr . "transform")
                      (trn . "transition")
                      (tr-o . "transform-origin")
                      (w-s . "white-space")
                      (w . "width")
                      (w-b . "word-break")
                      (w-w . "word-wrap")
                      (v . "visibility")
                      (vi . "visibility")
                      (v-a . "vertical-align")
                      (z . "z-index")
                      (z-i . "z-index")
                      ))
         (keyword (symbol-before-cursor))
         (found-property (cdr (assoc (intern keyword) inserters)))
         ;;found-property doesn't necessarily start with keyword (e.g. c=color but bc = background-color)
         (completion (concat found-property ": ")))
    (if found-property
        (progn
          (backward-delete-char (length keyword)); delete typed keyword
          (insert completion); cursor will be placed after the inserted text
          (save-excursion ; revert cursor to position before semicolon
            (insert ";")))
      (insert ":"))))

(defun autocomplete-css-value (&optional append-space)
  "When user types a keyword followed by a semicolon, autocomplete common css values"
  (interactive)
  (let* ((inserters '((a . "absolute")
                      (au . "auto")
                      (ba . "baseline")
                      (bas . "baseline")
                      (b . "block")
                      (bo . "bold")
                      (bol . "bold")
                      (bot . "bottom")
                      (both . "both")
                      (b-a . "break-all")
                      (b-w . "break-word")
                      (cap . "capitalize")
                      (c . "center")
                      (co . "column")
                      (col . "column")
                      (cov . "cover")
                      (d . "default")
                      (e . "text-overflow: ellipsis")
                      (el . "text-overflow: ellipsis")
                      (ell . "text-overflow: ellipsis")
                      (fi . "fixed")
                      (f . "flex")
                      (f-e . "flex-end")
                      (f-s . "flex-start")
                      (h . "hidden")
                      (i . "inherit")
                      (in . "inline")
                      (i-b . "inline-block")
                      (i-f . "inline-flex")
                      (it . "italic")
                      (j . "justify")
                      (k-a . "keep-all")
                      (l . "left")
                      (m . "middle")
                      (n . "none")
                      (nor . "normal")
                      (now . "nowrap")
                      (po . "pointer")
                      (re . "relative")
                      (r . "right")
                      (ro . "row")
                      (s . "stretch")
                      (sc . "scroll")
                      (sol . "solid")
                      (s-a . "space-around")
                      (s-b . "space-between")
                      (st . "static")
                      (te . "text")
                      (t . "top")
                      (tr . "transparent")
                      (u . "underline")
                      (un . "underline")
                      (up . "uppercase")
                      (v . "visible")
                      (vi . "visible")
                      (w . "wrap")
                      (abs . "@include abs(0)")
                      (absr . "@include absr(0)")
                      (absbl . "@include absbl(0)")
                      (absbr . "@include absbr(0)")
                      (abs0 . "@include abs(0)")
                      (abs100 . "@include abs(100%, 0)")
                      (absl100 . "@include abs(100%, 0)")
                      (absr100 . "@include absr(100%, 0)")
                      (abst100 . "@include abs(0, 100%)")
                      (absb100 . "@include absbl(100%, 0)")
                      (absbl100 . "@include absbl(100%, 0)")
                      (absbr100 . "@include absbr(100%, 0)")
                      (cover . "@include abs(0); @include size(100%)")
                      (abscover . "@include abs(0); @include size(100%)")
                      (size . "@include size(100%)")
                      (pse . "content: ''; display: block")
                      (af . "&:after { }")
                      (aft . "&:after { }")
                      (be . "&:before { }")
                      (bef . "&:before { }")
                      (bt . "border-top: 1px solid $gray-border")
                      (br . "border-right: 1px solid $gray-border")
                      (bb . "border-bottom: 1px solid $gray-border")
                      (bl . "border-left: 1px solid $gray-border")
                      (fc . "&:first-child { }")
                      (fi . "&:first-child { }")
                      (fo . "&:focus { }")
                      (ho . "&:hover { }")
                      (hov . "&:hover { }")
                      (hover . "&:hover { }")
                      (la . "&:last-child { }")
                      (lc . "&:last-child { }")
                      (nc . "&:nth-child() { }")
                      (nt . "&:nth-child() { }")
                      (nth . "&:nth-child() { }")
                      (odd . "&:nth-child(odd) { }")
                      (even . "&:nth-child(even) { }")
                      (b50 . "border-radius: 50%")
                      (br50 . "border-radius: 50%")
                      (img . "@include img(1)")
                      (imgc . "@include img-contain(1)")
                      (f-a . "@include flex-apart()")
                      (f-a-c . "@include flex-apart(center)")
                      (f-a-s . "@include flex-apart(stretch)")
                      (f-ce . "@include flex-center()")
                      (f-center . "@include flex-center()")
                      (f-c-c . "@include flex-col-center()")
                      (i-f-c . "@include inline-flex-center()")
                      (f-r . "@include flex-row()")
                      (f-c . "@include flex-col()")
                      (f-col . "@include flex-col()")
                      (rel . "@include rel(0)")
                      (relt . "@include relt(0)")
                      (relr . "@include relr(0)")
                      (relbl . "@include relbl(0)")
                      (relbr . "@include relbr(0)")
                      (rel0 . "@include rel(0)")
                      (fix . "@include fixed(0)")
                      (fixed . "@include fixed(0)")
                      (caps . "@include caps")
                      (fw . "font-weight: bold")
                      (icon . "@include icon()")
                      (pse-ci . "@include pseudo-circle()")
                      (ps-li . "@include pseudo-line(after, $color: )")
                      (ps-li . "@include pseudo-line(after, $color: )")
                      (rot . "transform: rotate(0deg)")
                      (t-l . "text-align: left")
                      (t-c . "text-align: center")
                      (t-r . "text-align: right")
                      (fw . "font-weight: bold")
                      (fwn . "font-weight: normal")
                      (fwb . "font-weight: bold")
                      (item . ".item- {
.item-img {

}
.item-details {

}
.item-title {

}
.item-desc {

}
}")
                      ))
         (is-important (equal "!" (buffer-substring (- (point) 1) (point))))
         (keyword (if is-important
                      (progn
                        ;;delete "!" so that symbol-before-cursor can find the keyword
                        ;;otherwise, it would return "!"
                        (if is-important (backward-delete-char 1))
                        (symbol-before-cursor))
                    (symbol-before-cursor)))
         (found-value (cdr (assoc (intern keyword) inserters)))
         (important-suffix (if is-important " !important" ""))
         (completion (concat found-value important-suffix)))
    (if found-value
        (progn
          ;;delete typed keyword
          (backward-delete-char (length keyword))
          ;;cursor will be placed after the inserted text
          (insert completion)))
    ;;add the semicolon or jump through if one's already after cursor
    (insert-semicolon-consider-existing append-space)))

(add-hook 'scss-mode-hook
          (lambda ()
            (local-set-key (kbd ":") 'autocomplete-css-property)))

(add-hook 'scss-mode-hook
          (lambda ()
            (local-set-key (kbd ";") (lambda ()
                                       (interactive)
                                       (autocomplete-css-value t)))))

(add-hook 'scss-mode-hook
          (lambda ()
            (local-set-key (kbd "C-;") (lambda ()
                                         (interactive)
                                         (autocomplete-css-value nil)))))

;; end CSS autocomplete


;;; BEGIN ESC keybindings (quick to use, intended to replace longer C-x keystrokes)
;;; also esc-arrows for navigating between split windows is located in #'set-windmove-keybindings
(global-set-key (kbd "<escape> k") 'kill-buffer)
(global-set-key (kbd "<escape> n") 'new-frame)
(global-set-key (kbd "<escape> 0") 'delete-window)
(global-set-key (kbd "<escape> <escape> 0") 'delete-frame)
(global-set-key (kbd "<escape> 5") 'delete-frame)
(global-set-key (kbd "<escape> q") 'ask-before-closing)
(global-set-key (kbd "<escape> e") 'eval-last-sexp)
(global-set-key (kbd "<escape> f") 'find-file)
(global-set-key (kbd "<escape> w") 'write-file)
(global-set-key (kbd "<escape> s") 'save-buffer)
(global-set-key (kbd "<escape> b") 'switch-to-buffer)
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
        (kt . ((dir . "/projects/kt/layout")
               (serve-cmd . "gulp serve")))
        (asb . ((dir . "/projects/asb/layout")
                (serve-cmd . "gulp serve")))
        (ici . ((dir . "/projects/ici")
                (serve-cmd . "npm run dev && open http://ici.devv")))
        (lw . ((dir . "/projects/lw/layout")
               (serve-cmd . "gulp webserver")))
        ;;(ald . ((dir . "/projects/ald")
        ;;        (serve-cmd . "gulp serve")))
        (ald . ((dir . "/projects/ald/layout")
                (serve-cmd . "gulp serve")))
        (bt . ((dir . "/projects/bt")
               (serve-cmd . "gulp serve")))
        (cx . ((dir . "/projects/cx/solution/helixcore.webapp")
               (serve-cmd . "gulp webserver")))
        (pn . ((dir . "/projects/pens")
               (serve-cmd . "open http://localhost:3000 && npm run dev")))
        (meo . ((dir . "/projects/meomari")
                (serve-cmd . "npm start")
                (left-window-file . "/src/index.js")
                (right-window-file . "/src/assets")))))

;; project-mode
(defun project-mode (project-name)
  (let* ((serve-buffer-name (concat (symbol-name project-name) "-serve"))
         (project-config (cdr (assoc project-name project-configs)))
         (project-dir (cdr (assoc 'dir project-config)))
         (serve-cmd (cdr (assoc 'serve-cmd project-config)))
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
    (delete-window)))

(defun lb-mode ()
  (interactive)
  (project-mode 'lb))

(defun bk-mode ()
  (interactive)
  (project-mode 'bk))

(defun kt-mode ()
  (interactive)
  (project-mode 'kt))

(defun asb-mode ()
  (interactive)
  (project-mode 'asb))

(defun lw-mode ()
  (interactive)
  (project-mode 'lw))

(defun ici-mode ()
  (interactive)
  (project-mode 'ici))

(defun ald-mode ()
  (interactive)
  (project-mode 'ald))

(defun bt-mode ()
  (interactive)
  (project-mode 'bt))

(defun cx-mode ()
  (interactive)
  (project-mode 'cx))

(defun pn-mode ()
  (interactive)
  (project-mode 'pn))

(defun meo-mode ()
  (interactive)
  (project-mode 'meo))

