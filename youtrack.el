(require 'url)

(defvar yt-user ""
  "Login user for youtrack.")

(defvar yt-password ""
  "Password for the youtrack user.")

(defvar yt-baseurl ""
  "Base url for youtrack.
Ex: https://bug.idvc.es")

(defvar yt-project ""
  "Default project shortname")

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

(defun yt-bug (project summary &optional description)
  "creates a youtrack issue"

  (interactive "sProj. Shortname: \nsSummary: \nsDesc: ")

  (defun login (baseurl user password)
    "authenticates with youtrack"
    (let
	((url-path "/rest/user/login"))
      (http-post (format "%s%s" baseurl url-path)
		 (list `("login" . ,user) `("password" . ,password)))))

  (if (eq description nil)
      (setq description ""))

  (let
      ((url-path "/rest/issue"))

    (login yt-baseurl yt-user yt-password)
    (http-put (concat yt-baseurl url-path)
		      (list `("project" . ,project) ; shortname of the project
			    `("summary" . ,summary)
			    `("description" . ,description)))))

(provide 'youtrack)
