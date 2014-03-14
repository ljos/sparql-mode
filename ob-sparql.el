;;; ob-sparql.el --- org-babel functions for SPARQL queries

;; Copyright (C) 2014 Bjarte Johansen

;; Author: Bjarte Johansen
;; Keywords: literate programming, reproducible research
;; Homepage: http://www.github.com/ljos/sparql-mode
;; Version: 0.0.1

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with SPARQL mode. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides a way to evaluate SPARQL queries in org-mode.

;;; Usage:

;; Add to your Emacs config:

;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((sparql . t)))

;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)
(require 'sparql-mode)

(defvar org-babel-default-header-args:sparql
  `((:url . ,sparql-default-base-url)
    (:format . ,sparql-default-format))
  "Default arguments for evaluating a SPARQL query
block.")

(defun org-babel-execute:sparql (body params)
  "Execute a block containing a SPARQL query with
org-babel.  This function is called by
`org-babel-execute-src-block'. If `es-warn-on-delete-query' is
set to true, this function will also ask if the user really wants
to do that."
  (message "Executing a SPARQL query block.")
  (let ((endpoint-url (cdr (assoc :url params)))
        (url-request-method "POST")
        (url-request-data (format "query=%s" (url-hexify-string body)))
        (url-request-extra-headers
         `(("Content-Type" . "application/x-www-form-urlencoded")
           ("Accept" . ,(cdr (assoc :format params))))))
    (with-current-buffer (url-retrieve-synchronously endpoint-url)
      (goto-char (point-min))
      (when (string-match "^.* 200 OK$" (thing-at-point 'line))
        (search-forward "\n\n")
        (delete-region (point-min) (point)))
      (buffer-string))))

(provide 'ob-sparql)
;;; ob-sparql.el ends here
