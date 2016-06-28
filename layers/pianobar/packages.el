;;; packages.el --- pianobar layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Chris Thompson <teaforthecat@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:


;;; Code:

(defconst pianobar-packages
  '(pianobar
    password-store)
  "Read by Spacemacs. These are the packages this layer 'owns'")

(defun pianobar-peek ()
  (interactive)
  (popwin:popup-buffer "*pianobar*"))

(defun pianobar/init-password-store ()
  "Ran by Spacemacs."
  (use-package password-store))

(defun pianobar/init-pianobar ()
  "Ran by Spacemacs."
  (use-package pianobar
    :defines (pianobar-mode-map)
    :bind-keymap ("<f7>" . pianobar-mode-map)
    :config
    ;; pianobar-mode-map doesn't exist until loaded, so this must be in :config
    (bind-keys :map pianobar-mode-map
               ("<f7>" . pianobar)
               ;; SPC w p p for unpeek

               ("p" . pianobar-peek)
               ("n" . pianobar-next-song)
               ("SPC" . pianobar-play-or-pause) )
    ;; this is in config so the user doesn't get prompted on startup
    (setq
     pianobar-run-in-background t
     pianobar-station "0" ;;override in spacemacs/user-config
     pianobar-username (password-store-get "pandora.com/username")
     pianobar-password (password-store-get "pandora.com/password"))))


;;; packages.el ends here
