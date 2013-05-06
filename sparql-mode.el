;;; sparql-mode.el --- Edit and interactively evaluate SPARQL queries.

;; Copyright (C) 2011  Craig Andera
;; Copyright (C) 2013  Marcus Nitzschke
;; Copyright (C) 2013  Bjarte Johansen

;; Author: Craig Andera <candera at wangdera dot com>
;; Maintainer: Bjarte Johansen <Bjarte dot Johansen at gmail dot com>
;; Homepage: https://github.com/ljos/sparql-mode
;; Version: 0.4.0

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

;; Mode for editing and interactively evaluating SPARQL queries.

;; Usage:

;; Add to your Emacs config:

;;  (add-to-list 'load-path "/path/to/sparql-mode-dir")
;;  (autoload 'sparql-mode "sparql-mode.el"
;;   "Major mode for editing SPARQL files" t)
;;  (add-to-list 'auto-mode-alist '("\\.sparql$" . sparql-mode))

;;; Code:

(defgroup sparql nil
  "Major mode for editing and evaluating SPARQL queries."
  :group 'languages)

(defcustom sparql-indent-offset 2
  "*Indentation offset for `sparql-mode'."
  :group 'sparql
  :type 'integer)

(defcustom sparql-default-base-url "http://localhost:2020/"
  "The default URL of the SPARQL endpoint."
  :group 'sparql
  :type 'string)

(defcustom sparql-default-format "text/csv"
  "The default format of the returned results."
  :group 'sparql
  :type '(choice
          (const :tag "Comma separated values" "text/csv")
          (const :tag "Tab separated values" "text/tab-separated-values")
          (const :tag "JSON" "application/sparql-results+json")
          (const :tag "SPARQL XML" "application/sparql-results+xml")
          (string :tag "Custom")))

(defcustom sparql-prompt-format nil
  "Non-nil means prompt user for requested format on each query
evaluation."
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

(defvar sparql-results-buffer nil)
(defvar sparql-base-url nil)
(defvar sparql-format nil)

(defun sparql-set-base-url (url)
  "Set the base URL for queries."
  ;; TODO: This isn't displaying the prompt for some reason
  (interactive "sNew base URL for queries: ")
  (setq sparql-base-url url))

(defun sparql-get-base-url ()
  "Returns the base URL for SPARQL queries in this buffer unless
it has not been set, in which case it prompts the user."
  (if sparql-base-url
      sparql-base-url
    (setq sparql-base-url
          (read-string
           (format "SPARQL URL (%s): " sparql-default-base-url)
           nil
           nil
           sparql-default-base-url))))

(defun sparql-get-format ()
  "Returns the requested result format for queries in this buffer
unless it has not been set, in which case it prompts the user."
  (let ((current-format (or sparql-format sparql-default-format)))
    (setq sparql-format
          (read-string
           (format "Format (%s): " current-format)
           nil
           nil
           current-format))))

(defvar sparql-result-response)

(defun sparql-handle-results (status &optional sparql-results-buffer)
  "Handles the results that come back from url-retrieve for a
SPARQL query."
  (let ((http-results-buffer (current-buffer)))
    (set-buffer sparql-results-buffer)
    (let ((buffer-read-only nil))
      (insert-buffer-substring http-results-buffer)
      (kill-buffer http-results-buffer)
      (delete-trailing-whitespace)
      (goto-char (point-min))
      (when (string-match "^.* 200 OK$" (thing-at-point 'line))
        (search-forward "\n\n")
        (setq sparql-result-response
              (buffer-substring (point-min) (point)))
        (delete-region (point-min) (point)))
      (setq mode-name "SPARQL[finished]"))))

(defun sparql-query-region ()
  "Submit the active region as a query to a SPARQL HTTP endpoint.
If the region is not active, use the whole buffer."
  (interactive)
  (let* ((beg (if (region-active-p) (region-beginning) (point-min)))
         (end (if (region-active-p) (region-end) (point-max)))
         (text (buffer-substring beg end))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/x-www-form-urlencoded")
            ("Accept" . ,(sparql-get-format))))
         (url-request-data (format "query=%s" (http-url-encode text)))
         (url (sparql-get-base-url)))
    (unless (buffer-live-p sparql-results-buffer)
      (setq sparql-results-buffer
            (generate-new-buffer (format "*SPARQL: %s*" (buffer-name)))))
    (save-current-buffer
      (set-buffer sparql-results-buffer)
      (sparql-result-mode)
      (let ((buffer-read-only nil))
        (delete-region (point-min) (point-max)))
      (setq buffer-read-only t))
    (url-retrieve url 'sparql-handle-results (list sparql-results-buffer))
    (view-buffer-other-window sparql-results-buffer)
    (other-window -1)))

(defconst sparql-keywords-re
  (regexp-opt
   '("ADD" "ALL" "AS" "ASC" "ASK"
     "BASE" "BIND" "BINDINGS" "BY"
     "CLEAR" "CONSTRUCT" "COPY" "CREATE"
     "DATA" "DEFAULT" "DELETE" "DESC" "DESCRIBE" "DISTINCT" "DROP"
     "EXISTS"
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
   'symbols))

(defconst sparql-keywords
  `(("<[^[:space:]]*>" . font-lock-constant-face)
    ("^[^#]*?\\(\"[^\"]*\"\\)" 1 font-lock-string-face)
    ("^[^#]*?\\('[^']*'\\)" 1 font-lock-string-face)
    (".*?\\(#.*\\)" 1 font-lock-comment-face)
    ,sparql-keywords-re
    ("[?$]\\w+" 0 font-lock-variable-name-face)))

(defun sparql-indent-line ()
  "Indent current line as a sparql expression."
  (interactive)
  (back-to-indentation)
  (let (indent-column)
    (save-excursion
      (forward-line -1)
      (setq indent-column
            (string-match "[^[:space:]]+\s+[^[:space:]]+;\s*$"
                          (thing-at-point 'line))))
    (save-excursion
      (ignore-errors
        (while (not indent-column)
          (backward-up-list)
          (cond ((looking-at "{")
                 (setq indent-column
                       (+ (current-indentation)
                          sparql-indent-offset)))
                ((looking-at "(")
                 (setq indent-column
                       (1+ (current-column))))))))
    (when (looking-at "}")
      (setq indent-column
            (- indent-column
               sparql-indent-offset)))
    (indent-line-to (or indent-column 0))))

(define-derived-mode sparql-result-mode prog-mode "SPARQL[waiting]"
  (make-local-variable 'sparql-result-response))

;;;###autoload
(define-derived-mode sparql-mode prog-mode "SPARQL"
  "Major mode for SPARQL-queries.
\\{sparql-mode-map}"
  :group 'sparql-mode
  (make-local-variable 'sparql-base-url)
  ;; Results buffer
  (make-local-variable 'sparql-results-buffer)
  ;; Comments
  (set (make-local-variable 'comment-start) "# ")
  ;; Indentation
  (set (make-local-variable 'indent-line-function) 'sparql-indent-line)
  ;; Font-lock support
  (setq font-lock-defaults
        '(sparql-keywords
          nil ;; font-lock-keywords-only
          t   ;; font-lock-keywords-case-fold-search
          ))
  ;; Modify syntax table to allow ?var to be accessed with
  ;; symbol-at-point and idle-highlight-mode will work without any
  ;; problem.
  (modify-syntax-entry ?? "w")
  ;; Key maps
  (define-key sparql-mode-map (kbd "C-c C-c") 'sparql-query-region))

(provide 'sparql-mode)

;;; sparql-mode.el ends here
