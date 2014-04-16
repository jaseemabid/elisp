(require 'url)
(require 'json)

(defvar yt-user ""
  "Login user for youtrack.")

(defvar yt-password ""
  "Password for the youtrack user.")

(defvar yt-baseurl ""
  "Base url for youtrack.
Ex: https://bug.idvc.es")

(defvar yt-project ""
  "Default project shortname")

(defvar yt-buffer "*youtrack*"
  "Name of the buffer to show the list of issues")

;; Helper methods to work on issues
(defun get-id (issue)
  "Return ID or nil given an issue"
  (let ((id nil))
    (maphash (lambda (k v)
               (progn
                 (if (string= k "id")
                     (setq id v)))
               ) issue)
    id))

(defun get-desc  (issue)
  "Return description or empty string given an issue"
  (let ((desc "")
        (i 0)
        (field (gethash "field" issue)))
    (while (< i (length field))
      (let ((prop (elt field i)))
        (if (string= "description" (gethash "name" prop))
            (setq desc (gethash "value" prop))))
      (incf i))
    desc))

(defun issue-format (issue)
  "Format given issue for list display

Pads the issue to 3 chars
Clips the issue description at 80 chars "
  (let ((id (get-id issue))
        (desc-maxlen 74)
        (desc (get-desc issue)))

    ;; If description spans multiple lines, show only till first \n
    (let ((multi (search "\n" desc)))
      (if multi
          (setq desc-maxlen (- multi 1))))

    (setq desc (substring desc 0 (min desc-maxlen (length desc))))
    (concat id "  \u00B7  " desc "\n")))

(defun http-post (url args)
  "Send ARGS to URL as a POST request."

  (let ((url-request-method "POST")
	(url-request-extra-headers
	 '(("Content-Type" . "application/x-www-form-urlencoded")))
	(url-request-data
	 (mapconcat (lambda (arg)
		      (concat (url-hexify-string (car arg))
			      "="
			      (url-hexify-string (cdr arg))))
		    args
                        "&")))

    (url-retrieve url 'dump-url-buffer)))

(defun http-put (url args)
  "Send ARGS to URL as a PUT request."

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
  "The buffer contains the raw HTTP response sent by the server."
  (switch-to-buffer (current-buffer))) ; use kill-buffer if you don't want to see response

(defun login (baseurl user password)
  "authenticates with youtrack"
  (let
      ((url-path "/rest/user/login"))
    (http-post (format "%s%s" baseurl url-path)
               (list `("login" . ,user) `("password" . ,password)))))

(defun yt-bug (project summary &optional description)
  "creates a youtrack issue"

  (interactive "sProj. Shortname: \nsSummary: \nsDesc: ")

  (if (eq description nil)
      (setq description ""))

  (let
      ((url-path "/rest/issue"))
    (login yt-baseurl yt-user yt-password)
    (http-put (concat yt-baseurl url-path)
		      (list `("project" . ,project) ; shortname of the project
                    `("summary" . ,summary)
                    `("description" . ,description)))))

(defun yt-issues-show (&optional project)
  "Lists youtrack issues for project

The issues are read from a issues.json and parsed to pretty print
the issues is a dedicated buffer"
  (interactive)
  (let ((json-object-type 'hash-table))
    (setq issues (json-read-file "./issues.json") )
    )

  (switch-to-buffer (get-buffer-create yt-buffer))
  (erase-buffer)

  (let ((i 0))
    (while (< i (length issues))
      (setq issue (elt issues i))
      (insert (issue-format issue))
      (incf i))))

(provide 'youtrack)
