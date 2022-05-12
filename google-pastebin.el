;;; google-pastebin.el --- Emacs interface for go/paste  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Copyright 2010 Google Inc.  All Rights Reserved.
;;
;; Simple interface for gpastebin ( http://go/paste )
;;
;; Author: Alexander Botero-Lowry <alexbl@google.com>

;;; Code:

(require 'google-prodaccess)

(defcustom gpastebin-language-mode-map '((java-mode . "java")
                                         (jde-mode . "java")
                                         (javascript-mode . "js")
                                         (js-mode . "js")
                                         (js2-mode . "js")
                                         (c++-mode . "cpp")
                                         (c-mode . "c")
                                         (python-mode . "py")
                                         (sh-mode . "shell"))
  "Map major mode names to syntax highlighting name."
  :type '(repeat (cons symbol string))
  :group 'google-tools)


(defgroup gpastebin
  nil
  "Google Pastebin go/gpaste"
  :prefix "gpastebin-"
  :group 'google)

(defcustom gpastebin-default-visibility 'private
  "Set default visibility for documents."
  :type '(choice (const :tag "Set Visibility to Private" private)
                 (const :tag "Set Visibility to Public" public))
  :group 'gpastebin)

(defun gpastebin-query-string (pairs)
  "Convert alist PAIRS into an application/x-www-form-urlencoded string."
  (mapconcat
   (lambda (x) (format "%s=%s"
                       (url-hexify-string (car x))
                       (url-hexify-string (cdr x)))
     ) pairs "&"))

;;;###autoload
(defun gpastebin-paste-dwim (title)
  "Paste the region or buffer with TITLE.

If the region is active then paste that region.  Otherwise, paste the whole
buffer."
  (interactive "sgPaste Title: ")
  (if (region-active-p)
      (gpastebin-paste-region (region-beginning) (region-end) title)
    (gpastebin-paste-buffer title)))

;;;###autoload
(defun gpastebin-paste-buffer (title)
  "Paste entire buffer with TITLE and add the URL to the kill ring."
  (interactive (let ((title (and current-prefix-arg
                                 (read-string "gPaste Title: " nil))))
                 (list title)))
  (gpastebin-paste-string (buffer-string) title))

;;;###autoload
(defun gpastebin-paste-region (beg end title)
  "Paste region from BEG to END with TITLE and add the URL to the kill ring."
  (interactive (let ((title (and current-prefix-arg
                                 (read-string "gPaste Title: " nil))))
                 (list (region-beginning) (region-end) title)))
  (gpastebin-paste-string (buffer-substring beg end) title)
  (if transient-mark-mode
      (setq deactivate-mark t)))

(defun gpastebin-lang-for-mode ()
  "Return the gpastebin syntax name for current major mode or \"text\"."
  (or (assoc-default major-mode gpastebin-language-mode-map) "text"))

(defun gpastebin-paste-string (paste-string &optional title)
  "Paste PASTE-STRING to gpastebin with TITLE and add the URL to the kill ring."
  ;; rpcget -blade_service apphosting-frontend
  ;;        -output_header http://paste.googleplex.com/create/ -post
  (google-prodaccess-check 0.12)
  (let ((url-request-data (gpastebin-query-string
                           (list (cons "text" paste-string)
                                 (cons "title" title)
                                 (cons "visibility"
                                       (symbol-name
                                        gpastebin-default-visibility))
                                 (cons "language" (gpastebin-lang-for-mode))))))
    (with-current-buffer (get-buffer-create "*gpastebin*")
      (erase-buffer)
      (insert url-request-data)
      (save-excursion
        (call-process-region
         (point-min)
         (point-max)
         "sso_client" nil (current-buffer) nil
         "--use_master_cookie"
         "-method" "POST"
         "-dump_header"
         "-data_file" "/dev/stdin"
         "-url" "https://paste.googleplex.com/create/"))
      (newline)
      (newline)
      (if (null (re-search-forward "^Location: " nil t))
          (error "Unable to create paste.  See %s for details" (current-buffer))
        (let ((paste-url (buffer-substring (point) (- (point-at-eol) 1))))
          (message paste-url)
          (kill-new paste-url))))))

(provide 'google-pastebin)

;;; google-pastebin.el ends here
