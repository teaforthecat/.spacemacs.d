(defmacro interact-with (name compilation)
  `(list ,compilation
         (switch-to-buffer-other-window "*compilation*")
         (shell-mode)
         (read-only-mode -1)
         (goto-char (point-max))
         (unwind-protect
             (rename-buffer ,name)
           (rename-uniquely))))

(defun this-dir-name ()
  (car (last (delete "" (split-string default-directory "/")))))

(defun spawn-shell (name &optional cmds)
  "Invoke shell in buffer NAME with optional list of COMMANDS
   example (spawn-shell \"*.emacs.d*\" '(\"ls\" \"file init.el\"))"
  (interactive (list (read-string "Name: " (concat "*" (this-dir-name) "*" ))))
  (let ((ss-buffer (or (get-buffer name)
                       (get-buffer-create (generate-new-buffer-name name)))))
    (switch-to-buffer ss-buffer)
    (shell ss-buffer)
    (dolist (c cmds)
      (process-send-string nil c )
      (comint-send-input))))

(defun remote-shell ( &optional host)
  (interactive)
  (let* ((remote-hostname (or host (s-chomp (shell-command-to-string "hostname"))))
         (remote-buffer-name (format "*%s*" (car (split-string remote-hostname "\\." ))))
         (default-directory (format "/ssh:%s:" remote-hostname)))
    (eshell)
    (insert-string (concat "cd " default-directory))
    (dired default-directory)
    (spawn-shell remote-buffer-name)))


;; the space is important here, the whole line must match
(setq tramp-passcode-prompt "Passcode: ")

;; for tramp-enter-synchronous-response
(setq tramp-blank-line-regexp "^$")

(defun enter-passcode (proc passcode)
  (process-send-string proc
                       (concat passcode
                               tramp-local-end-of-line)))

(defun tramp-enter-synchronous-response (proc vec)
  ;; if the "Passcode: " is not valid, Dell Defender will ask again for the
  ;; passcode using the prompt: Enter Synchronous Response:
  (if (save-excursion
        (with-current-buffer (process-buffer proc)
          ;; Dell Defender inserts control character "^M" at end of line so the
          ;; cursor is on a blank line after the prompt
          (forward-line -1)
          (looking-at "Enter Synchronous Response:")))
      (let ((passcode (read-string "Enter Synchronous Response: ")))
        (enter-passcode proc passcode))))

(defun tramp-two-factor-send-passcode (proc vec)
  (let* ((passcode (read-string "Passcode: ")))
    (enter-passcode proc passcode)))

