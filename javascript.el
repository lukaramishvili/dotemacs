
(defun setup-rjsx-mode ()
  "Setup rjsx mode, enable LSP, company, TIDE etc."
  (setq-local indent-line-function 'js-jsx-indent-line)
  (setq-local js-indent-level 2)
  (setup-tide-mode)
  (company-mode)
  ;; (prettier-js-mode)
  (emmet-mode))
(add-hook 'rjsx-mode-hook 'setup-rjsx-mode)


(defun setup-tide-mode ()
  "Setup Typescript IDE mode."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  ;; if using company-mode, uncomment this
  (company-mode +1)
  ;; javascript editing is easier with subword mode
  (subword-mode +1)
  )
;; format options -- full list at https://github.com/Microsoft/TypeScript/blob/v3.3.1/src/server/protocol.ts#L2858-L2890
(setq tide-format-options
      '(:indentSize 2 :tabSize 2
                    :insertSpaceAfterFunctionKeywordForAnonymousFunctions t
                    :placeOpenBraceOnNewLineForFunctions nil
                    :insertSpaceAfterOpeningAndBeforeClosingTemplateStringBraces nil
                    :placeOpenBraceOnNewLineForControlBlocks nil))
;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

;; enable debugger (TODO for now). has support for Javascript in Chrome/Firefox, PHP, Elixir, Go, Python and native GDB/LLDB for C and C++.
(dap-mode 1)
(dap-ui-mode 1)


(require 'ng2-mode)
;; there's ng2-ts-mode and ng2-html-mode (activated automatically).

(require 'lsp-mode)

(use-package lsp-ui :commands lsp-ui-mode)
(use-package company-lsp :commands company-lsp)

;; (require 'flycheck-flow)
;; ;; will only be enabled for files with a //@flow declaration at the first line and a .flowconfig in project root.
;; (add-hook 'js2-mode-hook 'flow-minor-enable-automatically)
;; ;; 
;; (with-eval-after-load 'flycheck
;;   (flycheck-add-mode 'javascript-flow 'flow-minor-mode)
;;   (flycheck-add-mode 'javascript-eslint 'flow-minor-mode)
;;   (flycheck-add-next-checker 'javascript-flow 'javascript-eslint))
;; (with-eval-after-load 'company
;;   (add-to-list 'company-backends 'company-flow))


;; Add LSP support for specific major modes: https://github.com/emacs-lsp/lsp-mode#adding-support-for-languages


(add-hook 'prog-mode #'lsp) ; doesn't do anything in ng2-*-mode or unsupported prog-mode derived modes
;; (add-hook 'ng2-mode #'lsp) didn't apply to html/ts sub-modes
;; using tide-mode for ng-* files, so commented below lines
;; (add-hook 'ng2-ts-mode #'lsp)
;; (add-hook 'ng2-html-mode #'lsp)

;; To add LSP support for additional modes, see https://github.com/emacs-lsp/lsp-mode#supported-languages

;; SASS/SCSS/etc: `npm install -g vscode-css-languageserver-bin`
(add-hook 'scss-mode-hook #'lsp)
;; HTML LSP server: `npm install -g vscode-html-languageserver-bin`
;; Uncomment for LSP in html-mode, but didn't really find it useful
;;(add-hook 'html-mode-hook #'lsp)
;; WARNING: use `npm i -g bash-language-server --unsafe-perm=true --allow-root`, NOT `npm i -g bash-language-server`
;; there's also support for PHP, C++, Elixir, Ocaml, Python, Haskell, Go and Vue

;; Turn on tide-mode in .component.html/.ts files
;; (add-hook 'ng2-mode-hook #'setup-tide-mode) didn't work
(add-hook 'ng2-ts-mode-hook #'lsp)
(add-hook 'ng2-ts-mode-hook #'setup-tide-mode)
(add-hook 'ng2-html-mode-hook #'setup-tide-mode)

(add-hook 'typescript-mode-hook #'lsp)
(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-hook 'typescript-mode-hook #'company-mode)



(defun setup-javascript-mode ()
  "Setup javascript mode."
	(local-set-key (kbd "C-c f") 'open-js-lambda-block)
  ;; javascript editing is easier with subword mode
  (subword-mode +1))

