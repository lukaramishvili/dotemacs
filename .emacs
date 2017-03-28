;;;;
;;;; Luka Ramishvili's .emacs file
;;;;

;;; use C-x C-e to reload any s-exp

;;; installed packages (copied from M-x package-list-packages)
;;;2048-game          20140704.... installed  play 2048 in Emacs
;;;auctex             11.87.7      installed  Integrated environment for *TeX*
;;;markdown-mode      20151214.... installed  Emacs Major mode for Markdown-formatted text files
;;;markdown-mode+     20120829.510 installed  extra functions for markdown-mode
;;;php-mode           20140502.... installed  Major mode for editing PHP code
;;;recentf-ext        20130130.... installed  Recentf extensions
;;;slime              20140702.... installed  Superior Lisp Interaction Mode for Emacs

(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

; hide annoying GNU ad (I thereby classify it as such)
(setq inhibit-startup-message t)
; clear *scratch* default contents
(setq initial-scratch-message nil)

;(cd "/projects/")

;(setq default-directory "/projects/")

(setq-default indent-tabs-mode nil)

(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed"
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to exit Emacs? "))
      (if (< emacs-major-version 22)
          (save-buffers-kill-terminal)
        (save-buffers-kill-emacs))
    (message "Canceled exit")))

(when window-system
  (global-set-key (kbd "C-x C-c") 'ask-before-closing)
  (global-set-key (kbd "s-q") 'ask-before-closing))

; stops selection with a mouse being immediately injected to the kill ring
(setq mouse-drag-copy-region nil)
; hide the toolbar (check if available, or signals error in terminal)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
; hide the menu (no benefits in hiding the menu on osx)
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
; hide the scrollbars, not using them anyway
; also check if available, or signals error in terminal
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
; show column numbers
(column-number-mode)

; switch () and []
(keyboard-translate ?\( ?\[) 
(keyboard-translate ?\[ ?\() 
(keyboard-translate ?\) ?\]) 
(keyboard-translate ?\] ?\))
; Also use Alt-[ as Alt-( and Alt-] as Alt-)
(global-set-key (kbd "M-[") 'insert-parentheses)
(global-set-key (kbd "M-]") 'move-past-close-and-reindent)

(setq mac-command-modifier 'control)
(setq mac-control-modifier 'super)
; there's also 'control (C-), 'meta (M-), 'super (S-) and 'hyper (H-)

(defun insert-double-quotes ()
  "Inserts double quotes and places the cursor between them"
  (interactive)
  (insert "\"")
  (save-excursion
    (insert "\"")))

(global-set-key (kbd "M-\"") 'insert-double-quotes)

(defun previous-window ()
  (interactive)
  (other-window -1))

;;;navigate between windows using C-*-tab
(global-set-key (kbd "<C-tab>") 'other-window)
(global-set-key (kbd "<C-S-tab>") 'previous-window)
(global-set-key (kbd "<C-S-iso-lefttab>") 'previous-window)
(global-set-key (kbd "M-±") 'previous-window);; same as M-S-§
(global-set-key (kbd "M-§") 'other-window)

(global-set-key (kbd "<C-M-tab>") 'next-buffer)
(global-set-key (kbd "<C-M-S-tab>") 'previous-buffer)

;(global-set-key (kbd "M-n") 'new-frame)
;(global-set-key (kbd "M-S-n") 'new-frame)


; make C-h backspace, and use super-h for help (ctrl-h on my Mac)
(global-set-key "\C-h" 'backward-delete-char-untabify)
(global-set-key (kbd "C-S-d") 'backward-delete-char-untabify)
; also use C-h for backspace in regex search
(define-key isearch-mode-map "\C-h" 'isearch-delete-char)
(global-set-key [(super h)] 'help-command)

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

; variations on Steve Yegge recommendations
(defun kill-current-word ()
  (interactive)
  ;in case cursor was at the end of current word, prevent jumping to next word's end
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
(defun kill-current-sexp ()
  (interactive)
  (backward-up-list)
  (kill-sexp))
(global-set-key (kbd "C-1") 'backward-kill-word)
(global-set-key (kbd "C-2") 'kill-current-word)
(global-set-key (kbd "C-3") 'kill-current-symbol)
(global-set-key (kbd "C-4") 'kill-current-line)
(global-set-key (kbd "C-5") 'kill-current-sexp)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
;; make Alt-h and Alt-Ctrl-h the same as Alt-Backspace
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "C-M-h") 'backward-kill-word)

(global-set-key (kbd "C-z") 'undo)
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

;; Command-Shift-Enter adds an indented line after current line and moves cursor there
(defun open-indented-line ()
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))
(global-set-key (kbd "<C-return>") 'open-indented-line)
(global-set-key (kbd "<C-M-return>") 'newline-and-indent)
;; C-S-return is vacant, use it for something

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
(global-set-key (kbd "C-{") (lambda () (interactive) (open-brackets-block t)))
;(global-set-key (kbd "C-M-{") (lambda () (interactive) (open-brackets-block nil)))
(global-set-key (kbd "C-M-{") (lambda () (interactive) (open-brackets-block-inline)))
(global-set-key (kbd "C-}") (lambda () (interactive) (close-brackets-block t)))
(global-set-key (kbd "C-M-}") (lambda () (interactive) (close-brackets-block nil)))

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

  (global-set-key [(super b)] 'windmove-left)
  (global-set-key [(super f)] 'windmove-right)
  (global-set-key [(super p)] 'windmove-up)
  ;; super-n is well used on new-frame, so use super-meta-n
  ;(global-set-key [(super n)] 'windmove-down)

  (global-set-key [(super meta b)] 'windmove-left)
  (global-set-key [(super meta f)] 'windmove-right)
  (global-set-key [(super meta p)] 'windmove-up)
  (global-set-key [(super meta n)] 'windmove-down)

  (global-set-key [(control super b)] 'windmove-left)
  (global-set-key [(control super f)] 'windmove-right)
  (global-set-key [(control super p)] 'windmove-up)
  (global-set-key [(control super n)] 'windmove-down)
  
  (progn
    (require 'shell)
    (define-key shell-mode-map (kbd "<C-left>") 'windmove-left)
    (define-key shell-mode-map (kbd "<C-right>") 'windmove-right)
    (define-key shell-mode-map (kbd "<C-up>") 'windmove-up)
    (define-key shell-mode-map (kbd "<C-down>") 'windmove-down)

    (global-set-key [(super b)] 'windmove-left)
    (global-set-key [(super f)] 'windmove-right)
    (global-set-key [(super p)] 'windmove-up)
    ;; super-n is well used on new-frame, so use super-meta-n
    ;(global-set-key [(super n)] 'windmove-down)

    (global-set-key [(super meta b)] 'windmove-left)
    (global-set-key [(super meta f)] 'windmove-right)
    (global-set-key [(super meta p)] 'windmove-up)
    (global-set-key [(super meta n)] 'windmove-down)

    (define-key shell-mode-map [(control super b)] 'windmove-left)
    (define-key shell-mode-map [(control super f)] 'windmove-right)
    (define-key shell-mode-map [(control super p)] 'windmove-up)
    (define-key shell-mode-map [(control super n)] 'windmove-down)))

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
; on load, show recent file list
(defun on-new-window ()
  (other-window 1)
  (show-recent-file-list))
(add-hook 'window-setup-hook 'show-recent-file-list)
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
(global-set-key (kbd "C-x C-a") 'show-recent-file-list)

; bind interactive regex search to C-M-r and C-M-s (add alt to search for regex)
(global-set-key (kbd "<C-M-r>") 'isearch-backward-regexp)
(global-set-key (kbd "<C-M-s>") 'isearch-forward-regexp)

; C-M-w copies to OS clipboard; C-M-y yanks from OS clipboard
(global-set-key "\C-\M-w" 'clipboard-kill-ring-save)
(global-set-key "\C-\M-y" 'clipboard-yank)

; maximize emacs frame on startup (X11-specific but I'm not using anything else)
(defun x11-maximize-frame ()
  "Maximize the current frame (to full screen)"
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0)))

;(x11-maximize-frame)

(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

(defun google (query)
  "googles a query"
  (interactive "sQuery:")
  (browse-url (concat "http://www.google.com/search?q=" query)))


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



;setq load-slime-by-hand t in .emacs on computers where you dont want auto slime
(when (and (boundp 'load-slime-by-hand) load-slime-by-hand)
  ;;if "cannot open load file, slime", then $ cd /usr/local && git clone https://github.com/slime/slime
  (add-to-list 'load-path "/usr/local/slime")
  (require 'slime)
  (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
  (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
  ;; Optionally, specify the lisp program you are using. Default is "lisp"
  (setq inferior-lisp-program "sbcl"))

;;; colors
;(set-background-color "#3f3f3f")
;(set-foreground-color "white")

;; themes
(add-to-list 'custom-theme-load-path "~/dotemacs/blackboard-theme")
(load-theme 'blackboard t)

;;; fonts
(set-default-font "DejaVu Sans Mono")

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



(when (require 'web-mode nil 'noerror)
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  ;;
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (setq web-mode-engines-alist
      '(("php"    . "\\.phtml\\'")
        ("blade"  . "\\.blade\\."))))

(when (require 'web-mode-edit-element nil 'noerror)
  (add-hook 'web-mode-hook 'web-mode-edit-element-minor-mode))


; open .scss and .sass files in css-mode
(add-to-list 'auto-mode-alist '(".blade.php" . html-mode))
(add-to-list 'auto-mode-alist '(".scss" . css-mode));css-mode or web-mode
(add-to-list 'auto-mode-alist '(".sass" . css-mode));css-mode or web-mode


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

(defun sgml-transpose-tags-around-cursor ()
  ;; transpose tags before and after cursor
  (interactive)
  ;; transpose only works when cursor is at first element's beginning
  (sgml-skip-tag-backward 1)
  (web-mode-element-transpose))
(defun sgml-delete-tag-backward ()
  ;; delete the tag before cursor
  (interactive)
  (let ((tag-start (point)))
    (sgml-skip-tag-backward 1)
    (kill-region tag-start (point))))
(defun sgml-delete-tag-forward ()
  ;; delete the tag before cursor
  (interactive)
  (let ((tag-end (point)))
    (sgml-skip-tag-forward 1)
    (kill-region tag-end (point))))
(defun sgml-select-tag-backward ()
  ;; select tag before cursor (includes any space between current cursor position and closing tag
  (interactive)
  (set-mark-command nil)
  (sgml-skip-tag-backward 1))
(defun sgml-select-tag-forward ()
  ;; select tag before cursor (includes any space between current cursor position and opening tag
  (interactive)
  (set-mark-command nil)
  (sgml-skip-tag-forward 1))
(defun sgml-duplicate-previous-tag ()
  ;; insert the contents of the tag before cursor at the current cursor position
  (interactive)
  ;; remember the current cursor position; we'll paste there
  (save-excursion
    ;; jump to the beginning of previous tag, select it, and copy
    (sgml-skip-tag-backward 1)
    (set-mark-command nil)
    (sgml-skip-tag-forward 1)
    (kill-ring-save (point) (mark)))
  (open-indented-line)
  (yank))
(defun sgml-duplicate-next-tag ()
  ;; insert the contents of the tag after cursor at the current cursor position
  (interactive)
  ;; remember the current cursor position; we'll paste there
  (save-excursion
    ;; jump to the beginning of previous tag, select it, and copy
    (sgml-skip-tag-forward 1)
    (set-mark-command nil)
    (sgml-skip-tag-backward 1)
    (kill-ring-save (point) (mark)))
  (open-indented-line)
  (yank))


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
  ;; go forward one tag
  (local-set-key (kbd "C-c C-f") 'sgml-skip-tag-forward)
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
  (local-set-key (kbd "C-c w") 'web-mode-element-wrap))

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
                      (b-s . "background-size")
                      (bo . "border")
                      (bor . "border")
                      (bo-c . "border-color")
                      (bor-c . "border-color")
                      (bo-s . "border-style")
                      (bor-s . "border-style")
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
                      (fl . "float")
                      (f-f . "font-family")
                      (f-s . "font-size")
                      (f-st . "font-style")
                      (f-w . "font-weight")
                      (h . "height")
                      (j-c . "justify-content")
                      (l . "left")
                      (l-s-t . "list-style-type")
                      (l-h . "line-height")
                      (lh . "line-height")
                      (m . "margin")
                      (m-t . "margin-top")
                      (m-r . "margin-right")
                      (m-b . "margin-bottom")
                      (m-l . "margin-left")
                      (min-w . "min-width")
                      (min-h . "min-height")
                      (max-w . "max-width")
                      (max-h . "max-height")
                      (o . "order")
                      (op . "opacity")
                      (ov . "overflow")
                      (o-x . "overflow-x")
                      (o-y . "overflow-y")
                      (p . "padding")
                      (p-t . "padding-top")
                      (p-r . "padding-right")
                      (p-b . "padding-bottom")
                      (p-l . "padding-left")
                      (p-e . "pointer-events")
                      (po . "position")
                      (r . "right")
                      (t-a . "text-align")
                      (t-d . "text-decoration")
                      (t-t . "text-transform")
                      (t . "top")
                      (w-s . "white-space")
                      (w . "width")
                      (v . "visibility")
                      (v-a . "vertical-align")
                      (z . "z-index")
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
                      (bas . "baseline")
                      (b . "block")
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
                      (fi . "fixed")
                      (f . "flex")
                      (f-e . "flex-end")
                      (f-s . "flex-start")
                      (h . "hidden")
                      (i . "inherit")
                      (in . "inline")
                      (i-b . "inline-block")
                      (it . "italic")
                      (j . "justify")
                      (l . "left")
                      (m . "middle")
                      (n . "none")
                      (nor . "normal")
                      (now . "nowrap")
                      (po . "pointer")
                      (re . "relative")
                      (r . "right")
                      (ro . "row")
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
                      ))
         (is-important (equal "!" (buffer-substring (- (point) 1) (point))))
         (keyword (if is-important
                      (substring (symbol-before-cursor) 0 -1)
                    (symbol-before-cursor)))
         (found-value (cdr (assoc (intern keyword) inserters)))
         (important-suffix (if is-important " !important" ""))
         (completion (concat found-value important-suffix)))
    (if found-value
        (progn
          ;;delete "!" (if present)
          (if is-important (backward-delete-char 1))
          ;;delete typed keyword
          (backward-delete-char (length keyword))
          ;;cursor will be placed after the inserted text
          (insert completion)))
    ;;add the semicolon or jump through if one's already after cursor
    (insert-semicolon-consider-existing append-space)))

(add-hook 'css-mode-hook
          (lambda ()
            (local-set-key (kbd ":") 'autocomplete-css-property)))

(add-hook 'css-mode-hook
          (lambda ()
            (local-set-key (kbd ";") (lambda ()
                                       (interactive)
                                       (autocomplete-css-value t)))))

(add-hook 'css-mode-hook
          (lambda ()
            (local-set-key (kbd "C-;") (lambda ()
                                         (interactive)
                                         (autocomplete-css-value nil)))))
