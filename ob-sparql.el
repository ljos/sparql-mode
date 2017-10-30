;;; ob-sparql.el --- org-babel functions for SPARQL queries

;; Copyright (C) 2014, 2015 Bjarte Johansen
;; Copyright (C) 2014       Jacek Grzebyta
;; Copyright (C) 2015       Alf Lervåg

;; Author: Bjarte Johansen
;; Keywords: literate programming, reproducible research
;; Homepage: http://www.github.com/ljos/sparql-mode
;; Version: 1.1.2

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
(require 'sparql-mode)

(declare-function org-table-convert-region "org-table" (beg0 end0 &optional separator))
(declare-function org-table-to-lisp "org-table" (&optional txt))

(defvar org-babel-default-header-args:sparql
  `((:url . ,sparql-default-base-url)
    (:format . ,sparql-default-format))
  "Default arguments for evaluating a SPARQL query block.")

(defun org-babel-execute:sparql (body params)
  "Execute a block containing a SPARQL query with org-babel.
This function is called by `org-babel-execute-src-block'."
  (message "Executing a SPARQL query block.")
  (let ((url (cdr (assoc :url params)))
        (format (cdr (assoc :format params)))
        (query (org-babel-expand-body:sparql body params)))
    (with-temp-buffer
      (sparql-execute-query query url format t)
      (org-babel-result-cond
       (cdr (assoc :result-params params))
       (buffer-string)
       (if (string-equal "text/csv" format)
           (org-babel-sparql-convert-to-table)
         (buffer-string))))))

(defun org-babel-sparql-convert-to-table ()
  "Convert the results buffer to an org-table."
  (org-table-convert-region (point-min) (point-max) '(4))
  (let ((table (org-table-to-lisp)))
    (cons (car table) (cons 'hline (cdr table)))))

(defun org-babel-expand-body:sparql (body params)
  "Expand BODY according to PARAMS, returning expanded body.
A variable is marked by the use of '?' or '$'; the marker is not
part of the variable name, thus '?x' and '$x' refer to the same
variable."
  (with-temp-buffer
    (insert body)
    (let ((case-fold-search nil)
          (case-replace nil))
      (dolist (pair (org-babel--get-vars params))
        (goto-char (point-min))
        (let ((regexp (concat "[$?]" (regexp-quote (format "%s" (car pair)))))
              (replacement (cdr pair)))
          (while (re-search-forward regexp nil t)
            (replace-match replacement nil nil)))))
    (buffer-string)))

(provide 'ob-sparql)
;;; ob-sparql.el ends here
