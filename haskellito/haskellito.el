;;hint: to see emacs lisp function source, use 
;;M-x find-function RET (function name) RET

;;open a .hs file in one buffer and GHCi in another
(defun haskellito-san (hs-file)
  (interactive
   (find-file-read-args "Haskell file: "
                        (confirm-nonexistent-file-or-buffer)))
  ;;(haskell-mode)
  (split-window-horizontally)
  (find-file hs-file)
  (other-window 1)
  (shell (current-buffer))
  (process-send-string nil "ghci\n")
  (process-send-string nil ":set prompt haskellito>\n"))
