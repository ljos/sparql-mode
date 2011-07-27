;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Interactively evaluate SPARQL
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(defvar sparql-default-base-url  "http://localhost:2020/")

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

(defun sparql-handle-results (status &optional sparql-results-buffer)
  "Handles the results that come back from url-retrieve for a
SPARQL query."
  (let ((http-results-buffer (current-buffer)))
    (set-buffer sparql-results-buffer)
    (let ((buffer-read-only nil))
      (delete-region (point-min) (point-max))
      (insert-buffer-substring http-results-buffer)
      (goto-char (point-min))))
  (view-buffer-other-window sparql-results-buffer))

(defun sparql-query-region ()
  "Submit the active region as a query to a SPARQL HTTP endpoint.
If the region is not active, use the whole buffer."
  (interactive)
  (let* ((beg (if (region-active-p) (region-beginning) (point-min)))
         (end (if (region-active-p) (region-end) (point-max)))
         (text (buffer-substring beg end))
         (escaped-text (http-url-encode text))
         ;; TODO: Stop hardcoding this at some point
         (url (format "%s?format=csv&query=%s"
                      (sparql-get-base-url) escaped-text)))
    (url-retrieve url #'sparql-handle-results (list sparql-results-buffer))))

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
     "UNDEF" "USING"
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
  ;; Results buffer
  (set (make-local-variable 'sparql-results-buffer)
       (generate-new-buffer (format "*SPARQL: %s*" (buffer-name))))
  (save-current-buffer
    (set-buffer sparql-results-buffer)
    (setq buffer-read-only t))
  ;; Comments
  (make-local-variable 'comment-start)
  (setq comment-start "# ")
  ;; Font-lock support
  (setq font-lock-defaults '(sparql-keywords))
  ;; Key maps
  (define-key sparql-mode-map (kbd "C-c C-x") 'sparql-query-region))

(provide 'sparql-mode)