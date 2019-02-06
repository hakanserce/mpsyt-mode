;;; mpsyt-model.el -- A major mode for interacting with mpsyt
;;; Commentary:
;;; This mode allows interacting with mpsyt

;;; Code:

(require 'comint)

(defvar mpsyt-mode-command "mpsyt"
  "The command to run mpsyt.")

(defvar mpsyt-mode-command-arguments '()
  "Command line arguments to pass to the mpsyt command.")

(defvar mpsyt-mode-map
  "Basic mode map for mpsyt-mode.")

(defun mpsyt-mode--define-action (action-symbol action-command action-doc)
  "Function to define an action (ACTION-SYMBOL) to send ACTION-COMMAND to mpsyt.  ACTION-DOC is the documentation for action."
  (eval `(defun ,action-symbol ()
    ,(format "Send a command %s to mpsyt." action-doc)
    (interactive)
    (mpsyt-mode--send-command ,action-command))))

(defun mpsyt-mode--send-command (command)
  "Sends a string COMMAND to the underlying mpsyt process."
  (insert command)
  (comint-send-input t t))

(defvar mpsyt-mode--playback-actions
  "Actions available during playback.")

(defvar mpsyt-mode-prompt-regexp "^>"
  "Prompt for mpsyt command.")

(defun mpsyt ()
  "Run mpsyt inside Emacs."
  (interactive)
  (let* ((mpsyt-program mpsyt-mode-command)
         (buffer (comint-check-proc "mpsyt")))
    (pop-to-buffer-same-window
     (if (or buffer (not (derived-mode-p 'mpsyt-mode))
             (comint-check-proc (current-buffer)))
         (get-buffer-create (or buffer "*mpsyt*"))
       (current-buffer)))
    (unless buffer
     (apply 'make-comint-in-buffer "mpsyt" buffer
             mpsyt-program mpsyt-mode-command-arguments)
      (mpsyt-mode))))


(defun mpsyt-mode--initialize ()
  "Helper function to initialize mpsyt."
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t)
  (setq comint-input-sender-no-newline t)
  (setq mpsyt-mode--playback-actions
      '((mpsyt-mode-seek-forward (kbd "ESC [ C") "seek forward")
       (mpsyt-mode-seek-backward (kbd "ESC [ D") "seek backward")
       (mpsyt-mode-next-track ">" "next track")
       (mpsyt-mode-previous-track "<" "previous track")
       (mpsyt-mode-pause " " "pause")))
  (mapc (lambda (alist) (apply 'mpsyt-mode--define-action alist)) mpsyt-mode--playback-actions)
  (setq mpsyt-mode-map (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    (define-key map "\t" 'completion-at-point)
    (define-key map (kbd "C-c C-n") 'mpsyt-mode-next-track)
    (define-key map (kbd "C-c C-p") 'mpsyt-mode-previous-track)
    (define-key map (kbd "C-c C-f") 'mpsyt-mode-seek-forward)
    (define-key map (kbd "C-c C-b") 'mpsyt-mode-seek-backward)
    map)))

(define-derived-mode mpsyt-mode comint-mode "mpsyt"
  "Major mode for `mpsyt'.


\\<mpsyt-mode-map>"
  nil "mpsyt"
  ;; set prompt
  (setq comint-prompt-regexp mpsyt-mode-prompt-regexp)
  ;; make read only
  (setq comint-prompt-read-only t)
  ;; this makes it so commands like M-{ and M-} works.
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'font-lock-defaults) '(mpsyt-mode--font-lock-keywords t))
  (set (make-local-variable 'paragraph-start) mpsyt-mode-prompt-regexp))

(defconst mpsyt-mode--keywords
  '("/" "." "user" "pl" "shuffle" "add" "vp" "ls" "open" "play" "view" "save"))

(defvar mpsyt-mode--font-lock-keywords
  (list
   ;; highlight reserved commands
   `(,(concat "\\_<" (regexp-opt mpsyt-mode--keywords) "\\_>") . font-lock-keyword-face))
  "Additional expressions to highlight in `mpsyt-mode'.")



;; this has to be done in a hook.
(add-hook 'mpsyt-mode-hook 'mpsyt-mode--initialize)

       
  
                         
(provide 'mpsyt-mode)

;;; mpsyt-mode.el ends here
