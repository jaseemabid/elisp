;;; youtrack.el --- Youtrack mode for emacs

;; Author: Jaseem Abid <jaseemabid@gmail.com>
;;         Satish Srinivasan <satish@ideadevice.com>
;; Created: Sun Apr 13 02:25:08 2014 +0530
;; Keywords: youtrack, bug db, issue tracker

;; This file is not part of GNU Emacs.

;; The MIT License (MIT)

;; Copyright (C) 2014 Jaseem Abid <jaseemabid@gmail.com>

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Install:

;; Refer to README

;;; Commentary:
;; Youtrack mode to work with Youtrack bug tracker from within Emacs

(require 'url)
(require 'json)
(require 's)
(require 'cl-lib)

;;; Code:

;;;; Youtrack Major Mode setup

(defvar yt-mode-hook nil)

(defvar yt-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "l") 'yt-issues)
    (define-key map (kbd "c") 'yt-bug)
    (define-key map (kbd "f") 'yt-fetch-issues)
    (define-key map (kbd "SPC") 'yt-preview-item)
    (define-key map (kbd "p") 'yt-preview-previous)
    (define-key map (kbd "n") 'yt-preview-next)
    (define-key map (kbd "<return>") 'yt-visit-item)
    map)
  "Key map for youtrack major mode.")

(define-derived-mode yt-mode special-mode "Youtrack"
  "Major mode for interacting with youtrack bug tracker from Emacs.

\\{yt-mode-map}"
  (buffer-disable-undo)
  (use-local-map yt-mode-map))

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

(defcustom yt-issue-buffer "*youtrack-issue*"
  "Name of the buffer to show a single issue."
  :group 'youtrack
  :type 'string)

(defcustom yt-issue-db "~/.emacs.d/youtrack/issues.json"
  "Default project JSON DB path."
  :group 'youtrack
  :type 'file)

;;;; Appearance settings

(defface yt-id
  '((((class color) (background light))
     :foreground "firebrick")
    (((class color) (background dark))
     :foreground "tomato"))
  "Face for the ID elements."
  :group 'youtrack)

(defface yt-header
  '((((class color) (background light))
     :foreground "orange")
    (((class color) (background dark))
     :foreground "orange"))
  "Face for the title elements."
  :group 'youtrack)

;;;; Behavior settings

(defcustom yt-buffer-switch-function 'pop-to-buffer
  "Function for `yt-issues-list' to use for switching to the list buffer.

The function is given one argument, the status buffer."
  :group 'youtrack
  :type '(radio (function-item switch-to-buffer)
                (function-item pop-to-buffer)
                (function :tag "Other")))

;;;; Helper methods to work on single issue

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

(defun get-assignee-name (issue)
  "Return assignee or empty string given an ISSUE."
  (let ((assignee "")
        (i 0)
        (field (gethash "field" issue)))
    (while (< i (length field))
      (let ((prop (elt field i)))
        (if (string= "Assignee" (gethash "name" prop))
            (setq assignee (gethash "value" prop))))
      (cl-incf i))
    (gethash "fullName" (elt assignee 0))))

(defun get-assignee-id (issue)
  "Return assignee or empty string given an ISSUE."
  (let ((assignee "")
        (i 0)
        (field (gethash "field" issue)))
    (progn
      (while (< i (length field))
        (let ((prop (elt field i)))
          (if (string= "Assignee" (gethash "name" prop))
              (setq assignee (gethash "value" prop))))
        (cl-incf i))
      (if (= (length assignee) 0)
          ""
        (gethash "value" (elt assignee 0))))))

(defun issue-format (issue)
  "Format given ISSUE for list display.

Current formatting include:
- Pads the issue to 8 chars
- Clips the issue summary at `summary-maxlen' chars"
  (let ((id (get-id issue))
        (summary-maxlen 74)
        (summary (get-summary issue)))

    ;; If summary spans multiple lines, show only till first \n
    (let ((multi (cl-search "\n" summary)))
      (if multi
          (setq summary-maxlen (- multi 1))))

    (setq summary (substring summary 0 (min summary-maxlen (length summary)))
          id (s-pad-right 5 " " id))

    (concat
     (propertize id 'font-lock-face 'yt-id)
     " * "
     summary "\n")))

;;;; Helper methods to work on collection of issues

(defun yt-issue-count-for (issues &optional user)
  "Returns number of ISSUES assigned to USER.

Argument PROJECT Defaults to `yt-user'."
  (let ((user (or user yt-user)))
    (loop
     for issue across issues
     count (string= (get-assignee-id issue) user)
     )))

;;;; Network helper functions

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

(defun http-get (url &optional args)
  "Send GET request to URL with arguments ARGS."
  (let ((url-request-method "GET")
        (url-request-extra-headers
         '(("Accept" . "application/json"))))
    (url-retrieve-synchronously url)))

;;;; Remote interaction functions

(defun yt-fetch-issues ()
  "Downloads issues list from youtrack and save to `yt-issue-db`."
  (interactive)
  (let* ((url-path "/rest/issue/byproject/")
         (url-params "?max=1000")
         (url-issue-list (concat yt-baseurl url-path yt-project url-params)))
    (progn
      ;; Login once, this looks like the place for that
      (yt-login yt-user yt-password yt-baseurl)
      ;; Will be nice if I don't have to switch to the buffer and close it
      ;; [todo] - Force open in fundamental mode
      (switch-to-buffer (http-get url-issue-list))
      ;; Clear header info, why is it even there?
      (delete-region (point-min) url-http-end-of-headers)
      (write-file yt-issue-db)
      (kill-buffer))))

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

;;;; Issue DB helpers functions
(setq yt-issue-db-cache nil)
(defun yt-get-issue-db (&optional project)
  "Return issue db for PROJECT.

Fetch if not found locally"
  ;; [todo] - Fetch issues if local cache is not found
  ;; [todo] - Memoize cleanly
  (or yt-issue-db-cache
      (let ((json-object-type 'hash-table))
        (progn
          ;; [todo] - Handle errors raised by JSON decoder
          (setq yt-issue-db-cache (json-read-file yt-issue-db))
          yt-issue-db-cache))))

;;;; Buffer/display helper functions

(defun yt-setup-buffer (action &optional buffer)
  "Setup buffer to show contents of ACTION and turn on `yt-mode'."
  (let ((buffer-switch-function yt-buffer-switch-function)
        (buffer (or buffer yt-buffer))
        (contents (funcall action))
        (inhibit-read-only t))
    (funcall buffer-switch-function (get-buffer-create buffer))
    (erase-buffer)
    (font-lock-mode t)
    (insert contents)
    (goto-char (point-min))
    (yt-mode)))

(defun yt-issues-list (&optional project)
  "List youtrack issues for PROJECT.

The issues are read from `yt-issue-db' and parsed to pretty print
the issues is a dedicated buffer"
  (apply 'concat (mapcar 'issue-format (yt-get-issue-db))))

;;;; Wrappers

(defun yt-issues-overview (&optional issues &optional project)
  "List youtrack status for PROJECT.

A general overview of the project is shown."
  (let* ((issues (or issues (yt-get-issue-db)))
        (template "Youtrack

Project        : %s
Total Issues   : %d
Assigned to me : %d
Remote         : %s
Last Updated   : %s

Shortcuts:

l : Show log
c : Create a bug
f : Fetch issues")
        (project (or project yt-project))
        (mine (yt-issue-count-for issues))
        (total (length issues))
        (mtime (nth 5 (file-attributes yt-issue-db))))
    (apply 'format (list template
                         project
                         total
                         mine
                         yt-baseurl
                         (format-time-string "%Y-%m-%d %T" mtime)))))

(defun yt-issue-overview (issue)
  "Pretty print a ISSUE."
  (let ((template "%s

Assignee       : %s
Severity       : %s
Type           : %s
Target Version : %s
State          : %s
Due Date       : %s

%s

%s")
        (id (propertize (get-id issue) 'font-lock-face 'yt-id))
        (assignee (get-assignee-name issue))
        (severity "*severity*")
        (type "*type*")
        (version "*version*")
        (state "*state*")
        (due "*due date*")
        (summary (propertize (get-summary issue) 'font-lock-face 'yt-header))
        (desc (get-desc issue)))
    (apply 'format (list template id assignee severity type
                         version state due summary desc))))

(defun yt-visit-item ()
  (interactive)
  ;; Handle issues for now
  (yt-setup-buffer '(lambda ()
                      (yt-issue-overview (yt-issue-at-point))) yt-issue-buffer))

(defun yt-preview-item ()
  (interactive)
  (yt-visit-item)
  (pop-to-buffer yt-buffer))

(defun yt-preview-next ()
  (interactive)
  (next-line)
  (yt-preview-item))

(defun yt-preview-previous ()
  (interactive)
  (previous-line)
  (yt-preview-item))

(defun yt-issue-at-point ()
  (interactive "P")
  (let ((n (line-number-at-pos)))
    (elt (yt-get-issue-db) (- n 1))))

;;;; Interactive commands

(defun yt-status ()
  "Init screen for youtrack-mode."
  (interactive)

  ;; make sure user variables are configured
  (if (or (s-blank? yt-user) (s-blank? yt-password) (s-blank? yt-baseurl))
      (error "Configure youtrack to get started"))

  ;; fetch issues if the local DB is not present
  (unless (file-readable-p yt-issue-db)
    (yt-fetch-issues))

  (yt-setup-buffer 'yt-issues-overview))

(defun yt-issues ()
  "Lists all issues."
  (interactive)
  (yt-setup-buffer 'yt-issues-list))

(defun yt-bug (project summary description)
  "Create a youtrack issue.
Argument PROJECT Shortname of the project at YouTrack.
Argument SUMMARY Issue summary.
Optional argument DESCRIPTION Issue description."
  (interactive "sProj. Shortname: \nsSummary: \nsDesc: ")

  (let ((url-path "/rest/issue"))
    (http-put (concat yt-baseurl url-path)
		      (list `("project" . ,yt-project)
                    `("summary" . ,summary)
                    `("description" . ,description)))))

;;;; Key bindings and aliases
(define-key global-map (kbd "C-c y") 'yt-status)

(provide 'youtrack)
;;; youtrack.el ends here
