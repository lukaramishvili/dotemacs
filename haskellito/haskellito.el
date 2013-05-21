;;open a .hs file in one buffer and GHCi in another
(defun haskellito-san (hs-file)
  (interactive "sHaskell File:")
  (split-window-horizontally)
  (find-file hs-file)
  (other-window 1)
  (shell)
  ;open GHCi here
  )
