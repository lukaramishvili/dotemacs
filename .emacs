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

(setq mac-command-modifier 'control)
(setq mac-control-modifier 'super)
; there's also 'control (C-), 'meta (M-), 'super (S-) and 'hyper (H-)

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


; make C-h backspace, and use super-h for help (ctrl-h on my Mac)
(global-set-key "\C-h" 'backward-delete-char-untabify)
; also use C-h for backspace in regex search
(define-key isearch-mode-map "\C-h" 'isearch-delete-char)
(global-set-key [(super h)] 'help-command)

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
(global-set-key (kbd "C-1") 'backward-kill-word)
(global-set-key (kbd "C-2") 'kill-current-word)
(global-set-key (kbd "C-3") 'kill-current-symbol)
(global-set-key (kbd "C-4") 'kill-current-line)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; Command-Shift-Enter adds an indented line after current line and moves cursor there
(defun open-indented-line ()
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))
(global-set-key (kbd "<C-S-return>") 'open-indented-line)
;; Command-{ opens a {\n cursor will be here \n} block at the end of the line
(defun open-brackets-block ()
  (interactive)
  (move-end-of-line 1)
  (insert "{")
  (newline-and-indent)
  (insert "}")
  (indent-for-tab-command)
  (previous-line 1)
  (open-indented-line))
(global-set-key (kbd "C-{") 'open-brackets-block)

(defun set-windmove-keybindings ()
  (dolist (key '("<C-left>" "<C-right>" "<C-up>" "<C-down>"))
    (global-unset-key (kbd key))
    (local-unset-key (kbd key)))
  
  (global-set-key (kbd "<C-left>") 'windmove-left)
  (global-set-key (kbd "<C-right>") 'windmove-right)
  (global-set-key (kbd "<C-up>") 'windmove-up)
  (global-set-key (kbd "<C-down>") 'windmove-down)

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

    (define-key shell-mode-map [(control super b)] 'windmove-left)
    (define-key shell-mode-map [(control super f)] 'windmove-right)
    (define-key shell-mode-map [(control super p)] 'windmove-up)
    (define-key shell-mode-map [(control super n)] 'windmove-down)))

(set-windmove-keybindings)


;;switch buffer now doesn't touch other windows, opens in same buffer
(global-set-key "\C-x\C-b" 'buffer-menu)

;;currently "Control" (corner key labeled fn) is mapped to "Alt" on my macbook, 
;;so I can't use s-`. also, "§" key is above the "Tab" key where ` should be
(global-set-key (kbd "C-§") 'other-frame)


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

; open .scss and .sass files in css-mode
(add-to-list 'auto-mode-alist '(".blade.php" . html-mode))
(add-to-list 'auto-mode-alist '(".scss" . css-mode))
(add-to-list 'auto-mode-alist '(".sass" . css-mode))

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

