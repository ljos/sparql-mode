;;; company-sparql.el --- Add company support for sparql mode

;; Copyright (C) 2017 Bjarte Johansen

;; Author: Bjarte Johansen <Bjarte dot Johansen at gmail dot com>
;; Homepage: https://github.com/ljos/sparql-mode
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

;; Functions to add company support to sparql-mode.

;;; Usage:

;; (eval-after-load 'company
;;  '(add-to-list 'company-backends 'company-sparql))
;; (eval-after-load 'company-keywords
;;  '(add-to-list 'company-keywords-alist `(sparql-mode . ,sparql--keywords)))

;;; Code:
(require 'cl-lib)
(require 'company)
(require 'url)
(require 'url-handlers)

(defvar company-sparql-prefix-namespaces nil)
(defvar company-sparql-use-prefixcc t)

;;;###autoload
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
              (dolist (prefix company-sparql-prefix-namespaces)
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

(provide 'company-sparql)
