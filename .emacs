(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
;; This from a japanese individual.  I hope it works.
(setq default-buffer-file-coding-system 'utf-8)
;; From Emacs wiki
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
;; MS Windows clipboard is UTF-16LE 
(set-clipboard-coding-system 'utf-16le-dos)

(setq inferior-lisp-program "c:/progra~2/clisp-2.49/clisp.exe")
(add-to-list 'load-path "c:/emacs-23.3/slime-2011-10-03/")
(require 'slime)
(slime-setup)

(add-to-list 'load-path "~/.emacs.d/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d//ac-dict")
(ac-config-default)

(setq auto-mode-alist
          (cons '("\\.ml[iyl]?$" .  caml-mode) auto-mode-alist))

;; path to ocaml-mode installation (extracted path)
(setq load-path (cons "c:/emacs-23.3/ocaml-mode" load-path))

(autoload 'caml-mode "ocaml" (interactive)
  "Major mode for editing Caml code." t)
(autoload 'camldebug "camldebug" (interactive) "Debug caml mode")

;(load "c:/emacs-23.3/nxhtml/autostart.el")

(global-ede-mode t)

(cd "d:/htdocs")

(setq default-directory "d:/htdocs")

(w32-register-hot-key [A-tab])