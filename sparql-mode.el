;;; sparql-mode.el --- Edit and interactively evaluate SPARQL queries.

;; Copyright (C) 2011       Craig Andera
;; Copyright (C) 2013       Marcus Nitzschke
;; Copyright (C) 2013--2016 Bjarte Johansen
;; Copyright (C) 2013       Robert Syme
;; Copyright (C) 2014       Alex Tucker
;; Copyright (C) 2014       Jacek Grzebyta
;; Copyright (C) 2016       Ole Jørgen Brønner

;; Author: Craig Andera <candera at wangdera dot com>
;; Maintainer: Bjarte Johansen <Bjarte dot Johansen at gmail dot com>
;; Homepage: https://github.com/ljos/sparql-mode
;; Version: 2.1.1
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

(defvar sparql-results-buffer nil)
(defvar sparql-base-url nil)
(defvar sparql-format nil)

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

(defun sparql-execute-query (query &optional synch url format)
  "Submit the given `query' string to the endpoint at the given
`url'.  `sparql-execute-query' inserts the result of the query
into the current buffer.  If `synch' is true the query is sent
synchronously otherwise it is asynchronously.  `format' specifies
the return format of the response from the server. Note: This
uses the the mime accept header to set the format and not all
sparql endpoints expect that."
  (let ((endpoint-url (or url (sparql-get-base-url)))
	(url-mime-accept-string (or format (sparql-get-format)))
	(url-request-method "POST")
	(url-request-extra-headers
	 '(("Content-Type" . "application/x-www-form-urlencoded")))
	(url-request-data (concat "query=" (url-hexify-string query)))
	(result-buffer (current-buffer)))
    (if synch
	(with-current-buffer (url-retrieve-synchronously endpoint-url)
	  (sparql-handle-results nil result-buffer))
      (url-retrieve endpoint-url
		    #'sparql-handle-results
		    (list result-buffer)))))

(defun sparql-query-region (&optional synch)
  "Submit the active region as a query to a SPARQL HTTP endpoint.
If the region is not active, use the whole buffer.  If a prefix
argument is given the command is run synchronously instead of
asynchronously."
  (interactive "P")
  (let* ((beg (if (region-active-p) (region-beginning) (point-min)))
         (end (if (region-active-p) (region-end) (point-max)))
         (query (buffer-substring-no-properties beg end)))
    (unless (and sparql-results-buffer
		 (buffer-live-p sparql-results-buffer))
      (setq sparql-results-buffer (get-buffer-create
				   (format "*SPARQL: %s*" (buffer-name))))
      (with-current-buffer sparql-results-buffer
	(sparql-result-mode)))
    (with-current-buffer sparql-results-buffer
      (let ((buffer-read-only nil))
	(delete-region (point-min) (point-max))
	(sparql-execute-query query synch)))
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
    (save-excursion
      (forward-line -1)
      (setq indent-column
            (string-match "\\S-+\\s-+\\S-+;\\s-*$"
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
		(save-excursion
		  (backward-up-list)
		  (current-column)))))
    (indent-line-to (or indent-column 0))))

(defconst sparql-keywords
  `(("<\\S-*>" 0 font-lock-constant-face t)
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
    syntax-table)
  "Syntax table for SPARQL-mode.")

(defun sparql-syntax-propertize-function (start end)
  (save-excursion
   (goto-char start)
   (while (search-forward "#" end t)
     (let ((property (get-text-property (point) 'face)))
       (unless (or (equal 'font-lock-constant-face property)
		   (member 'font-lock-constant-face property))
	 (backward-char)
	 (put-text-property (point)
			    (1+ (point))
			    'syntax-table
			    (string-to-syntax "<"))
	 (end-of-line)
	 (unless (eobp)
	   (put-text-property (point)
			      (1+ (point))
			      'syntax-table
			      (string-to-syntax ">"))))))))

(defvar sparql-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'sparql-query-region)
    (define-key map (kbd "C-c C-u") 'sparql-set-base-url)
    (define-key map (kbd "C-c C-f") 'sparql-set-format)
    map)
  "Keymap for `sparql-mode'.")

(defvar ac-source-sparql-mode
  `((candidates . ,sparql--keywords)))

(eval-after-load 'auto-complete-mode
  '(add-to-list 'ac-sources 'ac-source-sparql-mode))

(defvar sparql-prefix-namespaces nil)
(defvar company-sparql-use-prefixcc t)

(defun company-sparql (command &optional arg &rest ignored)
  "`company-mode' completion back-end for `sparql-mode'. Right
now it only completes prefixes, `company-keywords' takes care of
keywords."
  (interactive (list 'interactive))
  (cl-case command
    (init (with-current-buffer (get-buffer-create "*SPARQL PREFIX*")
            (when (zerop (buffer-size))
              (when company-sparql-use-prefixcc
                (let ((url-request-method "GET"))
                  (url-insert
                   (url-retrieve-synchronously
                    "http://prefix.cc/popular/all.file.sparql" t)))
                (goto-char (point-min))
                (while (search-forward "PREFIX " nil t)
                  (replace-match "")))
              (dolist (prefix sparql-prefix-namespaces)
                (insert prefix "\n"))
              (sort-lines nil (point-min) (point-max))
              (bury-buffer))))
    (interactive (company-begin-backend 'company-sparql))
    (prefix (and (eq major-mode 'sparql-mode)
                 (< 0 (buffer-size
                       (get-buffer "*SPARQL PREFIX*")))
                 (let ((case-fold-search t))
                   (looking-back "^\\s-*PREFIX \\(.*\\)"))
                 (match-string 1)))
    (candidates (cl-remove-if-not (lambda (c) (string-prefix-p arg c))
                                  (with-current-buffer (get-buffer "*SPARQL PREFIX*")
                                    (split-string (buffer-string) "\n" t))))
    (require-match 'never)))

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-sparql))
(eval-after-load 'company-keywords
  '(add-to-list 'company-keywords-alist `(sparql-mode . ,sparql--keywords)))

(define-derived-mode sparql-result-mode read-only-mode "SPARQL[waiting]"
  "Major mode to hold the result from the SPARQL-queries."
  :group 'sparql-result-mode)

;; Compatability with Emacs < 24
(defalias 'sparql-parent-mode
  (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

;;;###autoload
(define-derived-mode sparql-mode sparql-parent-mode "SPARQL"
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
  (set (make-local-variable 'syntax-propertize-function)
       #'sparql-syntax-propertize-function))

(provide 'sparql-mode)

;;; sparql-mode.el ends here
