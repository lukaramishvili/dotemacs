; try to improve slow performance on windows.
(setq w32-get-true-file-attributes nil)

(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq slime-net-coding-system 'utf-8-unix)
;; This from a japanese individual.  I hope it works.
(setq default-buffer-file-coding-system 'utf-8)
;; From Emacs wiki
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
;; MS Windows clipboard is UTF-16LE 
(set-clipboard-coding-system 'utf-16le-dos)

(cd "d:/htdocs/lisp/")

(setq default-directory "d:/htdocs/lisp/")

(column-number-mode)

(w32-register-hot-key [A-tab])

;;;;idk why I need upper things on Linux or FreeBSD.

(keyboard-translate ?\( ?\[) 
(keyboard-translate ?\[ ?\() 
(keyboard-translate ?\) ?\]) 
(keyboard-translate ?\] ?\))

(defun google (query)
  "googles a query"
  (interactive "sQuery:")
  (browse-url (concat "http://www.google.com/search?q=" query)))