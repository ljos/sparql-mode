;;; sparql-mode.el --- Edit and interactively evaluate SPARQL queries.

;; Copyright (C) 2011       Craig Andera
;; Copyright (C) 2013       Marcus Nitzschke
;; Copyright (C) 2013, 2014 Bjarte Johansen
;; Copyright (C) 2013       Robert Syme

;; Author: Craig Andera <candera at wangdera dot com>
;; Maintainer: Bjarte Johansen <Bjarte dot Johansen at gmail dot com>
;; Homepage: https://github.com/ljos/sparql-mode
;; Version: 0.8.3

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

;; If you want auto-complete support you can also add:

;;  (add-to-list 'ac-dictionary-files "/path/to/sparql-mode-dir/sparql-mode")
;;  (add-hook 'sparql-mode-hook 'auto-complete-mode)

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
      (when (string-match (rx bol (* not-newline) space "200 OK" eol)
                          (thing-at-point 'line))
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
  `((,(rx "<" (* (not space)) ">")
     . font-lock-constant-face)
    (,(rx bol (*? (not (in "#"))) (group "\"" (* (not (in "\""))) "\""))
     1 font-lock-string-face)
    (,(rx bol (*? (not (in "#"))) (group "'" (* (not (in "'"))) "'"))
     1 font-lock-string-face)
    (,(rx (*? not-newline) (group "#" (* not-newline)))
     1 font-lock-comment-face)
    ,sparql-keywords-re
    (,(rx (any "?$") (+ word))
     0 font-lock-variable-name-face)))

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
                          (thing-at-point 'line)))
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
      (indent-line-to (or indent-column 0)))))

(defun sparql-result-show-response ()
  "Shows the header of the response from the server in the
minibuffer."
  (interactive)
  (message sparql-result-response))

(defvar sparql-result-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c m") 'sparql-result-show-response)
    map)
  "Keymap for `sparql-result-mode'.")

(define-derived-mode sparql-result-mode text-mode "SPARQL[waiting]"
  "Major mode to hold the result from the SPARQL-queries.
\\{sparql-result-mode-map}"
  ;; The response header from the server.
  (make-local-variable 'sparql-result-response))

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
    (modify-syntax-entry ?\" "." syntax-table) ; " ; <-- Stop the string on github
    (modify-syntax-entry ?\' "." syntax-table)
    syntax-table)
  "Syntax table for SPARQL-mode.")

(defvar sparql-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'sparql-query-region)
    (define-key map (kbd "C-c u") 'sparql-set-base-url)
    (define-key map (kbd "C-c f") 'sparql-set-format)
    map)
  "Keymap for `sparql-mode'.")

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
          )))

(provide 'sparql-mode)

;;; sparql-mode.el ends here
