;;; google-prodaccess.el --- interact with gcert  -*- lexical-binding: t; -*-
;;
;; Copyright 2010 Google Inc. All Rights Reserved.
;;
;; Author: alexbl@google.com (Alex Botero-Lowry)
;; Author: jktomer@google.com (Jonathan Tomer)
;; Author: jrockway@google.com (Jonathan Rockway)
;;
;;
;;; Commentary:
;; M-x prodaccess gets you prodaccess.  M-x customize-group google-prodaccess
;; will show you some bells and whistles you can tweak.
;;
;; This library is called ‘prodaccess’ after the original tool to obtain LOAS
;; certificates.  The tool has since been replaced by gcert, but the old name
;; remains for compatibility reasons.
;;
;;
;;; History:
;;
;;
;;; Code:

(eval-when-compile
  (require 'custom)
  (require 'easy-mmode))

(require 'timer)

(defgroup google-prodaccess nil
  "Configuration for ‘google-show-prodaccess-mode’ and other
gcert-related features."
  :prefix "google-prodaccess-"
  :group 'google-tools)

(defcustom google-fetchotp-command nil
  "Command to fetch an OTP, or nil to prompt the user."
  :group 'google-prodaccess
  :type '(choice (string :tag "Shell command")
                 (const :tag "Prompt" nil)))

(make-obsolete-variable 'google-fetchotp-command "OTPs don’t exist any more."
                        "2020-08-18")

(defcustom google-prodaccess-args nil
  "Arguments to pass to gcert; for example, \\='(\"--prodssh\")."
  :group 'google-prodaccess
  :type '(repeat (string :tag "Argument")))

(defcustom google-prodaccess-prodcertstatus-args nil
   "Extra arguments to pass to prodcertstatus."
  :group 'google-prodaccess
  :type '(repeat (string :tag "Argument")))

(defun google-prodaccess-p (&optional hours-left)
  "Return if the user has a LOAS2 certificate valid for at least HOURS-LEFT."
  (let ((default-directory "/")) ;; pwd may not exist if loas is expired
    (eq 0 (apply #'call-process "gcertstatus" nil nil nil
                 (append
                  google-prodaccess-prodcertstatus-args
                  '("--minloglevel=1"
                    "--nocheck_ssh"
                    "--norpc_binarylog")
                  (when hours-left (list "--check_remaining"
                                         (format "%fh" hours-left))))))))

(defvar google-prodaccess-hook nil
  "Hook run after a successful credential gathering attempt.")

(defun google-prodaccess-time-left ()
  "Return the amount of time left on the user's LOAS2 cert.

Returns the number of seconds remaining, or nil if there is no valid cert."
  (with-temp-buffer
    (let ((default-directory "/"))  ; pwd may not exist if loas is expired
      (when (eq 0 (apply #'call-process "gcertstatus" nil '(t nil) nil
                         `(,@google-prodaccess-prodcertstatus-args
                           "--check_loas2" "--nocheck_ssh" "--format=brief"
                           "--minloglevel=1" "--norpc_binarylog")))
        (goto-char 1)
        (when (re-search-forward (rx bol "LOAS2 expires in "
                                     (group (+ digit)) "h "
                                     (group (+ digit)) ?m eol)
                                 nil :noerror)
          (* 60 (+ (* 60 (string-to-number (match-string-no-properties 1)))
                   (string-to-number (match-string-no-properties 2)))))))))

(defun google-prodaccess--wait-for-prompt-filter (process output)
  "Processes the output of the program `gcert' to determine when to prompt.
Argument PROCESS is the gcert process object.
Argument OUTPUT is the chunk of output printed by gcert."
  ;; TODO(alexbl): This process filter will not properly deal with situations
  ;; where the process output is separated between multiple flushes of stdout.
  ;; The check for the OTP prompt is also rather fickle.
  (condition-case nil
      (progn
        (cond ((string-match "SSO password for" output)
               (let ((sso-passwd (read-passwd
                                  (concat "SSO Password for "
                                          (user-login-name) ": " ))))
                 ;; handle C-g during password input; notice quit and rethrow
                 (when (and (null sso-passwd) quit-flag)
                   (signal 'quit nil))
                 (process-send-string process (concat sso-passwd "\n"))))
              ((string-match "Please touch your security key\\." output)
               (message "Please touch your security key."))
              ((string-match "Touch registered\\. Getting cookie\\." output)
               (message "Touch registered."))))

    ;; If the user C-g'd at any time here, kill the process completely.  The
    ;; setq quit-flag ensures that the outer accept-process-output call notices
    ;; we quit and properly rethrows the signal.
    (quit (progn (kill-process process)
                 (setq quit-flag t)))))

(defun fetchotp ()
  "Retrieve an OTP by running `google-fetchotp-command' or by prompting the user."
  (declare (obsolete "OTPs don’t exist any more." "2020-08-18"))
  (interactive)
  (if google-fetchotp-command
      (shell-command-to-string google-fetchotp-command)
    (concat (read-from-minibuffer "OTP: ") "\n")))

(defun make-prodaccess-process (&optional remote)
  "Return a running gcert process with necessary filters installed.
If REMOTE is non-nil and ‘default-directory’ is on a remote
machine, start the process on the same remote machine."
  (let ((gcert-process
         (let ((process-connection-type t) ; ensure a PTY is allocated
               (default-directory
                 ;; ensure we are somewhere that exists
                 (concat (and remote (file-remote-p default-directory)) "/")))
           (apply #'start-file-process
                  "google-prodaccess-gcert" "*prodaccess-debug*" "gcert"
                  google-prodaccess-args))))
    (set-process-filter gcert-process
                        #'google-prodaccess--wait-for-prompt-filter)
    (set-process-sentinel gcert-process
                          #'google-prodaccess--sentinel)
    gcert-process))

;;;###autoload
(defun google-prodaccess-get (&optional remote)
  "Obtain production credentials interactively.
With prefix argument (or, non-interactively, if REMOTE is
non-nil), obtain production credentials on the remote machine
specified by ‘default-directory’."
  (interactive "P")
  (let ((gcert-process (make-prodaccess-process remote)))
    (while (and (eq (process-status gcert-process) 'run)
                (accept-process-output gcert-process)))
    (when (eq (process-status gcert-process) 'run)
      (kill-process gcert-process))
    (and (eq (process-status gcert-process) 'exit)
         (zerop (process-exit-status gcert-process)))))

;;;###autoload
(define-obsolete-function-alias 'prodaccess #'google-prodaccess-get
  "2020-08-18")

;;;###autoload
(defalias 'gcert #'google-prodaccess-get)

(defun google-prodaccess-check (&optional hours-left)
  "Obtain production credentials if necessary.

If HOURS-LEFT is specified, refetch credentials unless the
existing credentials are valid for longer than that."
  (interactive "P")
  (if (google-prodaccess-p
       (when hours-left (prefix-numeric-value hours-left)))
      (when (called-interactively-p 'any)
        (minibuffer-message "LOAS certificate is still valid"))
    (google-prodaccess-get)))

(defcustom google-prodaccess-good-modeline-face '(:foreground "darkgreen")
  "Symbol or list defining a face for the gcert modeline section.
Used by mode `google-show-prodaccess-mode' when LOAS cert is good."
  :group 'google-prodaccess
  :type '(choice face sexp))

(defcustom google-prodaccess-close-modeline-face
  '(:foreground "orange3" :weight bold)
  "Symbol or list defining a face for the gcert modeline section.
Used by mode `google-show-prodaccess-mode' when LOAS cert is good but
expiring soon.

“Soon” is defined by the value of `google-prodaccess-close-threshold'."
  :group 'google-prodaccess
  :type '(choice face sexp))

(defcustom google-prodaccess-bad-modeline-face 'font-lock-warning-face
  "Symbol or list defining a face for the gcert modeline section.
Used by mode `google-show-prodaccess-mode' when LOAS cert is missing."
  :group 'google-prodaccess
  :type '(choice face sexp))

(defcustom google-prodaccess-close-threshold 0.5
  "Time in hours before considering gcert insufficient.
May be an integer or floating-point number."
  :group 'google-prodaccess
  :type 'number)

(defcustom google-prodaccess-status-show-thres "Always"
  "Threshold to show prodaccess status.
If set to a number, then only show status when the time left is less than this
number (in hours).
If set to a string, \"Close\" shows the status string if it expires soon.
\"Expired\" shows it when prodaccess is already expired.
\"Always\" or any other value will always show the status (default)."
  :group 'google-prodaccess
  :type '(radio (number)
                (const "Always")
                (const "Close")
                (const "Expired")))

(defvar google-prodaccess-status-string nil
  "String displayed in the modeline with `google-show-prodaccess-mode' enabled.
Do not set this variable directly; it is managed by
`google-prodaccess--refresh-modeline-string'.")
(put 'google-prodaccess-status-string 'risky-local-variable t)

(defvar google-prodaccess-refresh-timer nil)

(defvar google-prodaccess--keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] #'google-prodaccess-get)
    map)
  "Keymap for the mode line prompt of `google-show-prodaccess-mode'.")

(defun google-prodaccess--get-time-thres ()
  "Return the time threshold for showing prodaccess status.
Returns in seconds, or nil (always show)."
  (when google-prodaccess-status-show-thres
    (if (stringp google-prodaccess-status-show-thres)
        (cond ((string-equal google-prodaccess-status-show-thres "Close")
               (* google-prodaccess-close-threshold 3600))
              ((string-equal google-prodaccess-status-show-thres "Expired")
               1))
      (when (numberp google-prodaccess-status-show-thres)
        (* google-prodaccess-status-show-thres 3600)))))

(defun google-prodaccess--refresh-modeline-string ()
  "Update the modeline to reflect the state of the user's production credentials.
Called on a timer when `google-show-prodaccess-mode' is on."
  (let* ((timeleft (google-prodaccess-time-left))
         (time-thres (google-prodaccess--get-time-thres))
         (display-status (or (not time-thres) (not timeleft)
                             (< timeleft time-thres))))
    (setq google-prodaccess-status-string
          (when display-status
            (list " "
                  (if timeleft
                      (list :propertize
                            (format "loas %d:%02d"
                                    (/ timeleft 3600)
                                    (/ (% timeleft 3600) 60))
                            'face
                            (if (< timeleft
                                   (* 3600 google-prodaccess-close-threshold))
                                google-prodaccess-close-modeline-face
                              google-prodaccess-good-modeline-face))
                    `(:propertize "run gcert"
                                  local-map ,google-prodaccess--keymap
                                  mouse-face mode-line-highlight
                                  face ,google-prodaccess-bad-modeline-face)))))
    (force-mode-line-update t)))

(define-minor-mode google-show-prodaccess-mode
  "Toggle modeline display of your production access status.
With prefix ARG, show your LOAS status in the modeline iff ARG is positive."
  :group 'google-prodaccess
  :require 'google-prodaccess
  :global t
  (and google-prodaccess-refresh-timer
       (cancel-timer google-prodaccess-refresh-timer))
  (setq google-prodaccess-refresh-timer nil
        google-prodaccess-status-string nil)
  (or global-mode-string (setq global-mode-string '("")))
  (when google-show-prodaccess-mode
    (or (memq 'google-prodaccess-status-string global-mode-string)
        (setq global-mode-string
              (append global-mode-string '(google-prodaccess-status-string))))
    (setq google-prodaccess-refresh-timer
          (run-with-timer 0 60 'google-prodaccess--refresh-modeline-string))))

(defun google-prodaccess--sentinel (_process status)
  "Manage the exit of the gcert process.
Argument _PROCESS is the gcert process object.
Argument STATUS is its one-line Emacs process status."
  (if (string= status "finished\n")
      (progn
        (message "Certificate for gcert updated successfully!")
        (when google-show-prodaccess-mode
          (google-prodaccess--refresh-modeline-string)))
      (message "There was an error while updating your gcert certificate.")))

(provide 'google-prodaccess)
;;; google-prodaccess.el ends here
