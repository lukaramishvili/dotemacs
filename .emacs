;;;;
;;;; Luka Ramishvili's .emacs file
;;;;

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(cd "/projects/")

(setq default-directory "/projects/")

(column-number-mode)

(keyboard-translate ?\( ?\[) 
(keyboard-translate ?\[ ?\() 
(keyboard-translate ?\) ?\]) 
(keyboard-translate ?\] ?\))

(defun previous-window ()
  (interactive)
  (other-window -1))

;;;navigate between windows using C-*-tab
(global-set-key (kbd "<C-tab>") 'other-window)
(global-set-key (kbd "<C-S-tab>") 'previous-window)
(global-set-key (kbd "<C-S-iso-lefttab>") 'previous-window)

(global-set-key (kbd "<C-M-tab>") 'next-buffer)
(global-set-key (kbd "<C-M-S-tab>") 'previous-buffer)

;;switch buffer now doesn't touch other windows
(global-set-key "\C-x\C-b" 'buffer-menu)

(defun google (query)
  "googles a query"
  (interactive "sQuery:")
  (browse-url (concat "http://www.google.com/search?q=" query)))

;setq load-slime-by-hand t in .emacs on computers where you want
(when (and (boundp 'load-slime-by-hand) load-slime-by-hand)
  (add-to-list 'load-path "/usr/local/slime")
  (require 'slime)
  (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
  (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
  ;; Optionally, specify the lisp program you are using. Default is "lisp"
  (setq inferior-lisp-program "sbcl"))

(load "~/dotemacs/haskellito/haskellito.el")