;;; sparql-mode.el --- Edit and interactively evaluate SPARQL queries.

;; Copyright (C) 2011       Craig Andera
;; Copyright (C) 2013       Marcus Nitzschke
;; Copyright (C) 2013--2015 Bjarte Johansen
;; Copyright (C) 2013       Robert Syme
;; Copyright (C) 2014       Alex Tucker
;; Copyright (C) 2014       Jacek Grzebyta

;; Author: Craig Andera <candera at wangdera dot com>
;; Maintainer: Bjarte Johansen <Bjarte dot Johansen at gmail dot com>
;; Homepage: https://github.com/ljos/sparql-mode
;; Version: 0.10.1
;; Package-Requires: ((cl-lib "0.5"))

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

;; Usage:

;; Add to your Emacs config:

;;  (add-to-list 'load-path "/path/to/sparql-mode-dir")
;;  (autoload 'sparql-mode "sparql-mode.el"
;;   "Major mode for editing SPARQL files" t)
;;  (add-to-list 'auto-mode-alist '("\\.sparql$" . sparql-mode))

;;; Code:
(require 'cl-lib)
(require 'url-handlers)

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

(defun sparql-handle-results (status &optional sparql-results-buffer)
  "Handles the results that come back from url-retrieve for a
SPARQL query."
  (let ((results-buffer (current-buffer))
        (response (url-http-parse-response)))
    (when (zerop (buffer-size))
      (setq mode-name "SPARQL[error]")
      (error "URL '%s' is not accessible" endpoint-url))
    (with-current-buffer sparql-results-buffer
      (let ((buffer-read-only nil))
        (if (and (<= 200 response) (<= response 299))
            (url-insert results-buffer)
          (insert results-buffer))
        (setq mode-name "SPARQL[finished]")))))

(defun sparql-query-region ()
  "Submit the active region as a query to a SPARQL HTTP endpoint.
If the region is not active, use the whole buffer."
  (interactive)
  (let* ((beg (if (region-active-p) (region-beginning) (point-min)))
         (end (if (region-active-p) (region-end) (point-max)))
         (text (buffer-substring beg end))
         (url-request-method "POST")
         (url-mime-accept-string (sparql-get-format))
         (url-request-extra-headers
          `(("Content-Type" . "application/x-www-form-urlencoded")))
         (url-request-data (concat "query=" (url-hexify-string text)))
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

(defconst sparql-keywords
  `((,(rx "<" (* (not space)) ">")
     0 font-lock-constant-face t)
    (,(rx bol (*? (not (in "#"))) (group "\"" (* (not (in "\""))) "\""))
     1 font-lock-string-face)
    (,(rx bol (*? (not (in "#"))) (group "'" (* (not (in "'"))) "'"))
     1 font-lock-string-face)
    (,(rx (*? not-newline) (group "#" (* not-newline)))
     1 font-lock-comment-face)
    (,(rx (any "?$") (+ word))
     0 font-lock-variable-name-face)
    ,(regexp-opt sparql--keywords)))

(defun sparql-indent-line ()
  "Indent current line as a sparql expression."
  (interactive)
  (back-to-indentation)
  (let (indent-column)
    (save-excursion
      (forward-line -1)
      (setq indent-column
            (string-match (rx (+ (not space)) (+ space)
                              (+ (not space))
                              ";"
                              (* space)
                              eol)
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


(defvar sparql-result-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-r") 'sparql-result-show-response)
    map)
  "Keymap for `sparql-result-mode'.")

(define-derived-mode sparql-result-mode text-mode "SPARQL[waiting]"
  "Major mode to hold the result from the SPARQL-queries.
\\{sparql-result-mode-map}")

(defvar sparql-mode-syntax-table
  (let ((syntax-table (make-syntax-table)))
    ;; Let `?` and `_` be part of a word so that a variable will be
    ;; interpreted as a word.
    (modify-syntax-entry ?? "w" syntax-table)
    (modify-syntax-entry ?_ "w" syntax-table)

    ;; Comments
    (modify-syntax-entry ?# "<" syntax-table)
    (modify-syntax-entry ?\n ">" syntax-table)

    ;; make `"` and `'` be punctuations so we can do our own
    ;; font-locking.
    (modify-syntax-entry ?\" "." syntax-table)
    (modify-syntax-entry ?\' "." syntax-table)
    syntax-table)
  "Syntax table for SPARQL-mode.")

(defvar sparql-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'sparql-query-region)
    (define-key map (kbd "C-c C-u") 'sparql-set-base-url)
    (define-key map (kbd "C-c C-f") 'sparql-set-format)
    map)
  "Keymap for `sparql-mode'.")

(defvar ac-source-sparql-mode
  `((candidates . ,sparql--keywords)))

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
    (candidates (remove-if-not (lambda (c) (string-prefix-p arg c))
                               (with-current-buffer (get-buffer "*SPARQL PREFIX*")
                                 (split-string (buffer-string) "\n" t))))
    (require-match 'never)))

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
  (when (boundp 'auto-complete-mode)
    (add-to-list 'ac-sources 'ac-source-sparql-mode))
  (when (boundp 'company-mode)
    (add-to-list 'company-backends 'company-sparql)
    (add-to-list 'company-keywords-alist
                 `(sparql-mode . ,sparql--keywords))))

(provide 'sparql-mode)

;;; sparql-mode.el ends here
