;;; sparql-mode.el --- Edit and interactively evaluate SPARQL queries.

;; Copyright (C) 2011       Craig Andera
;; Copyright (C) 2013       Marcus Nitzschke
;; Copyright (C) 2013--2017 Bjarte Johansen
;; Copyright (C) 2013       Robert Syme
;; Copyright (C) 2014       Alex Tucker
;; Copyright (C) 2014       Jacek Grzebyta
;; Copyright (C) 2016       Ole Jørgen Brønner

;; Author: Craig Andera <candera at wangdera dot com>
;; Maintainer: Bjarte Johansen <Bjarte dot Johansen at gmail dot com>
;; Homepage: https://github.com/ljos/sparql-mode
;; Version: 4.0.0
;; Package-Requires: ((cl-lib "0.5") (emacs "25.1"))

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
;; along with SPARQL mode. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Mode for editing and interactively evaluating SPARQL queries.

;;; Usage:

;; Add to your Emacs config:
;;  (add-to-list 'load-path "/path/to/sparql-mode-dir")
;;  (autoload 'sparql-mode "sparql-mode.el"
;;   "Major mode for editing SPARQL files" t)
;;  (add-to-list 'auto-mode-alist '("\\.sparql$" . sparql-mode))
;;  (add-to-list 'auto-mode-alist '("\\.rq$" . sparql-mode))

;;; Code:
(require 'cl-lib)
(require 'url)
(require 'url-vars)
(require 'url-parse)
(require 'url-handlers)

(eval-when-compile
  (require 'cl))

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

(defcustom sparql-prompt-base-url nil
  "Non-nil means prompt user for requested URL on each query
  evaluation."
  :group 'sparql
  :type 'boolean)

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

(defvar-local sparql-results-buffer nil)
(defvar-local sparql-base-url nil)
(defvar-local sparql-format nil)

(defvar sparql-base-url-history (list sparql-default-base-url))

(defun sparql-set-base-url (new-url)
  "Set the base URL for queries."
  (interactive
   (let ((current-url (or sparql-base-url sparql-default-base-url)))
     (list (read-string (format "SPARQL URL (%s): " current-url)
                        nil
                        'sparql-base-url-history
                        current-url))))
  (setq sparql-base-url
        (if (string= "" new-url)
            (or sparql-base-url sparql-default-base-url)
          (add-to-list 'sparql-base-url-history new-url)
          new-url)))

(defun sparql-get-base-url ()
  "Returns the base URL for SPARQL queries in this buffer unless
it has not been set, in which case it prompts the user."
  (or (and (not sparql-prompt-base-url) sparql-base-url)
      (command-execute 'sparql-set-base-url)))

(defvar sparql-format-history
  '("text/csv"
    "text/tab-separated-values"
    "application/sparql-results+json"
    "application/sparql-results+xml"))

(defun sparql-set-format (new-format)
  "Set the format that the server should respond in."
  (interactive
   (let ((current-format (or sparql-format sparql-default-format)))
     (list (read-string (format "Format (%s): " current-format)
                        nil
                        'sparql-format-history
                        current-format))))
  (setq sparql-format
        (if (string= "" new-format)
            (or sparql-format sparql-default-format)
          (add-to-list 'sparql-format-history new-format)
          new-format)))

(defun sparql-get-format ()
  "Returns the requested result format for queries in this buffer
unless it has not been set, in which case it prompts the user."
  (or (and (not sparql-prompt-format) sparql-format)
      (command-execute 'sparql-set-format)))

(defun sparql-handle-results (status &optional output-buffer)
  "Handles the result that comes back from url-retrieve for a
SPARQL query."
  (when (zerop (buffer-size))
    (setq mode-name "SPARQL[error]")
    (error "URL '%s' is not accessible"
           (url-recreate-url url-current-object)))
  (let ((results-buffer (current-buffer))
        (response (url-http-parse-response)))
    (with-current-buffer output-buffer
      (let ((buffer-read-only nil))
        (if (and (<= 200 response) (<= response 299))
            (url-insert results-buffer)
          (insert-buffer-substring results-buffer))
        (setq mode-name "SPARQL[finished]")))))

(defun sparql-execute-query (query url format synch)
  "Submit the given `query' string to the endpoint at the given
`url'.  `sparql-execute-query' inserts the result of the query
into the current buffer.  If `synch' is true the query is sent
synchronously otherwise it is asynchronously.  `format' specifies
the return format of the response from the server. Note: This
uses the the mime accept header to set the format and not all
sparql endpoints expect that."
  (let ((url-mime-accept-string (or format (sparql-get-format)))
        (url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/x-www-form-urlencoded")))
        (url-request-data (concat "query=" (url-hexify-string query)))
        (result-buffer (current-buffer)))
    (if synch
        (with-current-buffer (url-retrieve-synchronously url)
          (sparql-handle-results nil result-buffer))
      (url-retrieve url #'sparql-handle-results (list result-buffer)))))

(defun sparql-query-region (&optional synch)
  "Submit the active region as a query to a SPARQL HTTP endpoint.
If the region is not active, use the whole buffer.  If a prefix
argument is given the command is run synchronously instead of
asynchronously."
  (interactive "P")
  (let* ((beg (if (region-active-p) (region-beginning) (point-min)))
         (end (if (region-active-p) (region-end) (point-max)))
         (query (buffer-substring-no-properties beg end))
         (url (sparql-get-base-url))
         (format (sparql-get-format)))
    (unless (and sparql-results-buffer
                 (buffer-live-p sparql-results-buffer))
      (setq sparql-results-buffer (get-buffer-create
                                   (format "*SPARQL: %s*" (buffer-name))))
      (with-current-buffer sparql-results-buffer
        (sparql-result-mode)))
    (with-current-buffer sparql-results-buffer
      (let ((buffer-read-only nil))
        (delete-region (point-min) (point-max))
        (sparql-execute-query query url format synch)))
    (view-buffer-other-window sparql-results-buffer)
    (other-window -1)))

(defconst sparql--keywords
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
    "VALUES"
    "WHERE" "WITH")
  'symbols)

(defun sparql-indent-line ()
  "Indent current line as a sparql expression."
  (interactive)
  (back-to-indentation)
  (let (indent-column)
    (save-mark-and-excursion
      (forward-line -1)
      (setq indent-column
            (string-match "\\S-+\\s-+\\S-+;\\s-*$"
                          (thing-at-point 'line))))
    (save-mark-and-excursion
      (ignore-errors
        (while (not indent-column)
          (backward-up-list)
          (cond ((looking-at "{")
                 (setq indent-column
                       (+ (current-indentation)
                          sparql-indent-offset)))
                ((looking-at "(")
                 (setq indent-column
                       (1+ (current-column))))
                ((looking-at "\\[")
                 (forward-char)
                 (skip-syntax-forward " " (line-end-position))
                 (setq indent-column (+ (current-column)
                                        (if (eolp)
                                            (1- sparql-indent-offset)
                                          0))))))))
    (cond ((looking-at "}")
           (setq indent-column
                 (- indent-column
                    sparql-indent-offset)))
          ((looking-at "]")
           (setq indent-column
                 (save-mark-and-excursion
                   (backward-up-list)
                   (current-column)))))
    (indent-line-to (or indent-column 0))))

(defconst sparql-keywords
  `(("<\\S-*>" 0 font-lock-constant-face)
    ("[$?]\\w+" 0 font-lock-variable-name-face)
    ,(concat "\\b" (regexp-opt sparql--keywords) "\\b")))

(defvar sparql-mode-syntax-table
  (let ((syntax-table (make-syntax-table)))
    ;; Let `?` and `_` be part of a word so that a variable will be
    ;; interpreted as a word.
    (modify-syntax-entry ?? "w" syntax-table)
    (modify-syntax-entry ?_ "w" syntax-table)

    ;; Strings
    (modify-syntax-entry ?\' "\"'"  syntax-table)
    (modify-syntax-entry ?\" "\"\"" syntax-table)

    ;; Comments
    (modify-syntax-entry ?\n ">" syntax-table)
    ;; See `sparql-syntax-propertize-function' for comment beginnings.
    syntax-table)
  "Syntax table for SPARQL-mode.")

(defvar sparql-syntax-propertize-function
  (syntax-propertize-rules
   ("<\\S-*>" (0 "@"))
   ("#"       (0 "<")))
  "We define a `syntax-propertize-function' that skips URLs
because they can contain a #, but then adds the comment text
property for all other #.  See `sparql-mode-syntax-table' for the
definition of end of comment.")

(defvar sparql-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'sparql-query-region)
    (define-key map (kbd "C-c C-u") 'sparql-set-base-url)
    (define-key map (kbd "C-c C-f") 'sparql-set-format)
    map)
  "Keymap for `sparql-mode'.")

(defvar ac-source-sparql-mode
  `((candidates . ,sparql--keywords)))

(with-eval-after-load "auto-complete-mode"
  (add-to-list 'ac-sources 'ac-source-sparql-mode))

(define-derived-mode sparql-result-mode read-only-mode "SPARQL[waiting]"
  "Major mode to hold the result from the SPARQL-queries."
  :group 'sparql-result-mode)

;;;###autoload
(define-derived-mode sparql-mode prog-mode "SPARQL"
  "Major mode for SPARQL-queries.
\\{sparql-mode-map}"
  :group 'sparql-mode
  ;; Comments
  (setq-local comment-start "# ")
  ;; Indentation
  (setq-local indent-line-function 'sparql-indent-line)
  ;; Font-lock support
  (setq font-lock-defaults
        '(sparql-keywords
          nil ;; font-lock-keywords-only
          t   ;; font-lock-keywords-case-fold-search
          ))
  (setq-local syntax-propertize-function sparql-syntax-propertize-function))

(provide 'sparql-mode)

;;; sparql-mode.el ends here
