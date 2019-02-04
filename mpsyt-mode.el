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
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    (define-key map "\t" 'completion-at-point)
    (define-key map "\C-p" '(comint-send-input t t))
    (define-key map (kbd "C-M-p") '(message "Hello!"))
    map)
  "Basic mode map for mpsyt-mode.")

(defvar mpsyt-mode-prompt-regexp "^>"
  "Prompt for mpsyt command.")

(defun mpsyt-mode-run-mpsyt ()
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
  (setq comint-input-sender-no-newline t))


(define-derived-mode mpsyt-mode comint-mode "mpsyt"
  "Major mode for `run-mpsyt'.


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

       
  
                         


;;; mpsyt-mode.el ends here
