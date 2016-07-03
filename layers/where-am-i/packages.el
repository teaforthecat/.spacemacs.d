;;; packages.el --- where-am-i layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Chris Thompson <cthompson@teaforthecat.local>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

; Copies the path to the current buffer, with line number, into kill-ring.
; Meant for sharing links to code in a chat window.

;;; Code:

(defun minus-user-path (path)
  (let ((in-project-path (s-chop-prefix (projectile-project-root) path)))
    (s-concat (projectile-project-name) "/" in-project-path)))

(defun where-am-i ()
  "copies present location into kill-ring and clipboard"
  (interactive)
  (kill-new (message (concat (minus-user-path buffer-file-name) ":" (int-to-string (line-number-at-pos))))))
