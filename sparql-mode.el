;;; sparql-mode.el --- Interactively evaluate SPARQL

;; Copyright (C) 2011  Craig Andera
;; Copyright (C) 2013  Marcus Nitzschke

;; Author: Craig Andera <candera@wangdera.com>
;; Version: 0.0.1

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Usage:

;; Add to your emacs config:

;;  (add-to-list 'load-path "/path/to/sparql-mode-dir")
;;  (autoload 'sparql-mode "sparql-mode.el"
;;   "Major mode for editing SPARQL files" t)
;;  (add-to-list 'auto-mode-alist '("\\.sparql$" . sparql-mode))


(defgroup sparql nil
  "Major mode for editing and evaluating SPARQL queries."
  )

(defcustom sparql-default-base-url "http://localhost:2020/"
  "The default URL of the SPARQL endpoint."
  :group 'sparql
  :type 'string)

(defcustom sparql-default-format "csv"
  "The default format of the returned results."
  :group 'sparql
  :type 'string)

(defcustom sparql-prompt-format nil
  "Non-nil means prompt user for requested format on each query evaluation."
  :group 'sparql
  :type 'boolean)

;; URL encoding for parameters
(defun http-url-encode (str)
  "URL encode STR."
  (apply 'concat
          (mapcar (lambda (c)
                       (if (or (and (>= c ?a) (<= c ?z))
                                  (and (>= c ?A) (<= c ?Z))
                                     (and (>= c ?0) (<= c ?9)))
                                  (string c)
                              (format "%%%02x" c)))
                   (encode-coding-string str 'utf-8))))

(defvar sparql-base-url nil)
(defvar sparql-format nil)

(defun sparql-set-base-url (url)
  "Sets the base URL for queries"
  ;; TODO: This isn't displaying the prompt for some reason
  (interactive "sNew base URL for queries: ")
  (setq sparql-base-url url))

(defun sparql-get-base-url ()
  "Returns the base URL for SPARQL queries in this buffer unless it has not been set, in which case it prompts the user."
  (if sparql-base-url
      sparql-base-url
    (setq sparql-base-url
          (read-string
           (format "SPARQL URL (%s): " sparql-default-base-url)
           nil
           nil
           sparql-default-base-url))))

(defun sparql-get-format ()
  "Returns the requested result format for queries in this buffer unless it has not been set, in which case it prompts the user."
  (if sparql-format
      (setq sparql-format
	    (read-string
	     (format "Format (%s): " sparql-format)
	     nil
	     nil
	     sparql-format))
    (setq sparql-format
	  (read-string
	   (format "Format (%s): " sparql-default-format)
	   nil
	   nil
	   sparql-default-format))))

(defun sparql-query-region ()
  "Submit the active region as a query to a SPARQL HTTP endpoint.
If the region is not active, use the whole buffer."
  (interactive)
  (let* ((beg (if (region-active-p) (region-beginning) (point-min)))
         (end (if (region-active-p) (region-end) (point-max)))
         (text (buffer-substring beg end))
         (escaped-text (http-url-encode text))
         ;; TODO: Stop hardcoding this at some point
         (url (format "%s?format=%s&query=%s"
                      (sparql-get-base-url)
		      (if sparql-prompt-format (sparql-get-format) sparql-default-format)
		      escaped-text))
         (b (url-retrieve url
                          #'(lambda (status &rest cbargs)))))
    (switch-to-buffer-other-window b)))

(defconst sparql-keywords-re
  (regexp-opt
   '("ADD" "ALL" "AS" "ASC" "ASK"
     "BASE" "BIND" "BINDINGS" "BY"
     "CLEAR" "CONSTRUCT" "COPY" "CREATE"
     "DATA" "DEFAULT" "DELETE" "DESC" "DESCRIBE" "DISTINCT" "DROP"
     "FILTER" "FROM"
     "GRAPH" "GROUP"
     "HAVING"
     "IN" "INSERT" "INTO"
     "LIMIT" "LOAD"
     "MINUS" "MOVE"
     "NAMED" "NOT"
     "OFFSET" "OPTIONAL" "ORDER"
     "PREFIX"
     "REDUCED"
     "SELECT" "SERVICE" "SILENT"
     "TO"
     "UNDEF" "UNION" "USING"
     "WHERE" "WITH")
   'words))

(defconst sparql-keywords
  `(("<.*>" . font-lock-constant-face)
    ("#.*$" . font-lock-comment-face)
    ,sparql-keywords-re
    ("\\?\\w+" . font-lock-variable-name-face)
    ("\"[^\"]*\"" . font-lock-string-face)
    ("'[^']*'" . font-lock-string-face)))

(define-derived-mode sparql-mode text-mode
  "SPARQL"
  :group 'sparql-mode
  (make-local-variable 'sparql-base-url)
  ;; Comments
  (make-local-variable 'comment-start)
  (setq comment-start "# ")
  ;; Font-lock support
  (setq font-lock-defaults '(sparql-keywords))
  ;; Key maps
  (define-key sparql-mode-map (kbd "C-c C-x") 'sparql-query-region))

(provide 'sparql-mode)

;;; sparql-mode.el ends here
