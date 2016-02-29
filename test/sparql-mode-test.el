;;; sparql-mode-test.el --- SPARQL mode: Unit test suite

;; Copyright (C) 2014-2015 Bjarte Johansen <Bjarte.Johansen@gmail.com>

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

;; The unit test suite of SPARQL mode

;;; Code:

(require 'cl)
(require 'ert)
(require 'sparql-mode)

(ert-deftest sparql-test-execute-query ()
    "Send an asynchrounous query to a sparql endpoint."
    (with-temp-buffer
      (let ((sparql-results-buffer (current-buffer))
	    status
	    result)
	(cl-letf (((symbol-function 'sparql-handle-results)
		   (lambda (&rest _)
		     (let ((res (current-buffer)))
		       (setq status (url-http-parse-response))
		       (unless (zerop (buffer-size))
			 (with-temp-buffer
			   (url-insert res)
			   (goto-char (point-min))
                           (kill-whole-line)
			   (setq result (buffer-string))))))))
	  (with-timeout (10 (ert-fail "Could not wait for query anymore.")))
	  (let ((proc (get-buffer-process
		       (sparql-execute-query
			"SELECT ?Concept WHERE {[] a ?Concept} LIMIT 1"
			nil
			"http://dbpedia.org/sparql/"
			"application/sparql-results+json"))))
	    (while (not status)
	      (accept-process-output proc 1))))
	(should (and (<= 200 status 299)
		     (with-temp-buffer
		       (insert result)
                       (goto-char (point-min))
		       (json-read)))))))


(ert-deftest sparql-test-sych-execute-query ()
  "Send synchronous query to sparql endpoint."
  (let (status)
    (cl-letf (((symbol-function 'sparql-handle-results)
	       (lambda (&rest _)
		 (setq status (url-http-parse-response)))))
      (sparql-execute-query
       "SELECT ?Concept WHERE {[] a ?Concept} LIMIT 1"
       t
       "http://dbpedia.org/sparql/"
       "text/csv")
      (should (<= 200 status 299)))))
