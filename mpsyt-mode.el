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
   (nconc (make-sparse-keymap) comint-mode-map)
  "Basic mode map for mpsyt-mode.")

(defun mpsyt-mode--send-command (command)
  "Sends a string COMMAND to the underlying mpsyt process."
  (insert command)
  (comint-send-input t t))

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

(defvar mpsyt-mode--up (kbd "ESC [ A"))
(defvar mpsyt-mode--down (kbd "ESC [ B"))
(defvar mpsyt-mode--right (kbd "ESC [ C"))
(defvar mpsyt-mode--left (kbd "ESC [ D"))

(defun mpsyt-mode--init-playback-actions ()
  "Set playback actions."
  (let ((actions
        '(("seek-forward" "<right>" mpsyt-mode--right)
          ("seek-backward" "<left>" mpsyt-mode--left)
          ("pause" "SPC" " ")
          ("next-track" ">" ">")
          ("prev-track" "<" "<"))))
    (mapc (lambda (alist) (apply 'mpsyt-mode--define-action alist)) actions)))

(defun mpsyt-mode--define-action (action-name action-key action-mpsyt-command)
  "Function to define an action (ACTION-NAME) to send ACTION-MPSYT-COMMAND to mpsyt, and bind it to ACTION-KEY."
  (let ((action-command-symbol (intern (format "mpsyt-mode-%s" action-name)))
        (map mpsyt-mode-map))
    (eval `(defun ,action-command-symbol ()
             ,(format "Send a command %s to mpsyt." action-mpsyt-command)
             (interactive)
             (mpsyt-mode--send-command ,action-mpsyt-command)
             (set-temporary-overlay-map
              (let ((short-repeat-map (make-sparse-keymap)))
                (define-key short-repeat-map ,(kbd action-key) ',action-command-symbol)
                short-repeat-map))))
    (define-key map (kbd (format "C-c %s" action-key)) action-command-symbol)))
  
(defun mpsyt-mode--initialize ()
  "Helper function to initialize mpsyt."
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t)
  (setq comint-input-sender-no-newline t)
  (mpsyt-mode--init-playback-actions))


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
