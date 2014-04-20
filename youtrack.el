;;; youtrack.el --- Youtrack mode for emacs

(require 'url)
(require 'json)
(require 's)
(require 'cl-lib)

;;; Settings
;;;; Custom Groups

(defgroup youtrack nil
  "Controlling YouTrack from Emacs."
  :group 'tools)

(defcustom yt-user ""
  "Login user for youtrack."
  :group 'youtrack
  :type 'string)

(defcustom yt-password ""
  "Password for the youtrack user."
  :group 'youtrack
  :type 'string)

(defcustom yt-baseurl ""
  "Base url for youtrack.
Ex: https://bug.idvc.es"
  :group 'youtrack
  :type 'string)

(defcustom yt-project ""
  "Default project shortname."
  :group 'youtrack
  :type 'string)

(defcustom yt-buffer "*youtrack*"
  "Name of the buffer to show the list of issues."
  :group 'youtrack
  :type 'string)

;; Appearance settings
(defface yt-id
  '((((class color) (background light))
     :foreground "firebrick")
    (((class color) (background dark))
     :foreground "tomato"))
  "Face for the ID element of the issues list output."
  :group 'youtrack)

(defcustom yt-buffer-switch-function 'pop-to-buffer
  "Function for `yt-issues-list' to use for switching to the list buffer.

The function is given one argument, the status buffer."
  :group 'youtrack
  :type '(radio (function-item switch-to-buffer)
                (function-item pop-to-buffer)
                (function :tag "Other")))

;; Helper methods to work on issues
(defun get-id (issue)
  "Return ID or nil given an ISSUE."
  (let ((id nil))
    (maphash (lambda (k v)
               (progn
                 (if (string= k "id")
                     (setq id v)))
               ) issue)
    id))

(defun get-desc  (issue)
  "Return description or empty string given an ISSUE."
  (let ((desc "")
        (i 0)
        (field (gethash "field" issue)))
    (while (< i (length field))
      (let ((prop (elt field i)))
        (if (string= "description" (gethash "name" prop))
            (setq desc (gethash "value" prop))))
      (cl-incf i))
    desc))

(defun get-summary (issue)
  "Return summary or empty string given an ISSUE."
  (let ((summary "")
        (i 0)
        (field (gethash "field" issue)))
    (while (< i (length field))
      (let ((prop (elt field i)))
        (if (string= "summary" (gethash "name" prop))
            (setq summary (gethash "value" prop))))
      (cl-incf i))
    summary))

(defun issue-format (issue)
  "Format given ISSUE for list display.

Current formatting include:
- Pads the issue to 8 chars
- Clips the issue summary at 'summary-maxlen' chars"
  (let ((id (get-id issue))
        (summary-maxlen 74)
        (summary (get-summary issue)))

    ;; If summary spans multiple lines, show only till first \n
    (let ((multi (cl-search "\n" summary)))
      (if multi
          (setq summary-maxlen (- multi 1))))

    (setq summary (substring summary 0 (min summary-maxlen (length summary)))
          id (s-pad-right 8 " " id))

    (concat
     (propertize id 'font-lock-face 'yt-id)
     summary "\n")))

(defun http-post (url args)
  "Send POST request to URL with arguments ARGS."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/x-www-form-urlencoded")))
        (url-request-data (mapconcat (lambda (arg)
                                       (concat (url-hexify-string (car arg))
                                               "="
                                               (url-hexify-string (cdr arg))))
                                     args "&")))
    (url-retrieve url 'dump-url-buffer)))

(defun http-put (url args)
  "Send PUT request to URL with arguments ARGS."
  (setq args (mapconcat (lambda (arg)
	       (concat (url-hexify-string (car arg))
		       "="
		       (url-hexify-string (cdr arg))))
	     args
	     "&"))

  (print (format "PUT %s" (concat url "?" args)))
  (let ((url-request-method "PUT")
	(url-request-extra-headers '(("Content-Length" . "0"))))
    (url-retrieve (concat url "?" args) 'dump-url-buffer)))

(defun dump-url-buffer (status)
  "The buffer contain the raw HTTP response sent by the server.

[todo] - STATUS is ignored?"
  ;; use kill-buffer if you don't want to see response
  (switch-to-buffer (current-buffer)))

(defun yt-login (user password baseurl)
  "Authenticates USER with PASSWORD at BASEURL."
  (let
      ((url-path "/rest/user/login"))
    (http-post (format "%s%s" baseurl url-path)
               (list `("login" . ,user) `("password" . ,password)))))

(defun yt-bug (project summary &optional description)
  "Create a youtrack issue.
Argument PROJECT Shortname of the project at YouTrack.
Argument SUMMARY Issue summary.
Optional argument DESCRIPTION Issue description."

  (interactive "sProj. Shortname: \nsSummary: \nsDesc: ")

  (if (eq description nil)
      (setq description ""))

  (let
      ((url-path "/rest/issue"))
    (yt-login yt-baseurl yt-user yt-password)
    (http-put (concat yt-baseurl url-path)
		      (list `("project" . ,project) ; shortname of the project
                    `("summary" . ,summary)
                    `("description" . ,description)))))

(defun yt-setup-buffer (action)
  "Setup buffer to show contents of ACTION."

  (let ((buffer-switch-function yt-buffer-switch-function))
    (funcall buffer-switch-function (get-buffer-create yt-buffer))
    (font-lock-mode t))

  (erase-buffer)
  (insert (funcall action)))

(defun yt-issues-list (&optional project)
  "List youtrack issues for PROJECT.

The issues are read from a issues.json and parsed to pretty print
the issues is a dedicated buffer"
  (let ((json-object-type 'hash-table))
    ;; [todo] - Handle errors raised by JSON decoder
    (setq issues (json-read-file "./issues.json")))
  (apply 'concat (mapcar 'issue-format issues)))


(defun yt-status ()
  "Init screen for youtrack-mode

Aliased to issues list till something better comes along"
  (interactive)
  (yt-setup-buffer 'yt-issues-list))

;; Key bindings and aliases
(define-key global-map (kbd "C-c y") 'yt-status)

(provide 'youtrack)
;;; youtrack.el ends here
