;;;;
;;;; Luka Ramishvili's .emacs file
;;;;

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
; hide the toolbar
(tool-bar-mode -1)
; hide the menu (no benefits in hiding the menu on osx)
;(menu-bar-mode -1)
; show column numbers
(column-number-mode)

; switch () and []
(keyboard-translate ?\( ?\[) 
(keyboard-translate ?\[ ?\() 
(keyboard-translate ?\) ?\]) 
(keyboard-translate ?\] ?\))

(setq mac-command-modifier 'control)
(setq mac-control-modifier 'meta)

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

(defun set-windmove-keybindings ()
  (dolist (key '("<C-left>" "<C-right>" "<C-up>" "<C-down>"))
    (global-unset-key (kbd key))
    (local-unset-key (kbd key)))
  
  (global-set-key (kbd "<C-left>") 'windmove-left)
  (global-set-key (kbd "<C-right>") 'windmove-right)
  (global-set-key (kbd "<C-up>") 'windmove-up)
  (global-set-key (kbd "<C-down>") 'windmove-down)
  
  (progn
    (require 'shell)
    (define-key shell-mode-map (kbd "<C-left>") 'windmove-left)
    (define-key shell-mode-map (kbd "<C-right>") 'windmove-right)
    (define-key shell-mode-map (kbd "<C-up>") 'windmove-up)
    (define-key shell-mode-map (kbd "<C-down>") 'windmove-down)))

(set-windmove-keybindings)


;;switch buffer now doesn't touch other windows, opens in same buffer
(global-set-key "\C-x\C-b" 'buffer-menu)

;;currently "Control" (corner key labeled fn) is mapped to "Alt" on my macbook, 
;;so I can't use s-`. also, "§" key is above the "Tab" key where ` should be
(global-set-key (kbd "C-§") 'other-frame)


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
