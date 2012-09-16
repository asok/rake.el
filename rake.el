(eval-when-compile
  (require 'cl))

(defun rake-get-raw-tasks-string ()
  (message "Getting list of rake tasks...")
  (shell-command-to-string "rake --tasks --silent"))

(defun rake-extract-task-name (line)
  (when (string-match "^\\(.+?\\)\s+?# .+" line)
    (match-string 1 line)))

(defun rake-get-list-of-task-lines (raw-tasks-list)
  (loop
     for line in (split-string raw-tasks-list "\n")
     if (string-match-p "^rake " line)
     collect (replace-regexp-in-string "^rake " "" line)))

(defun rake-select-documented-task ()
  (let* ((tasks (rake-get-list-of-task-lines
                 (rake-get-raw-tasks-string)))
         (selected-row (ido-completing-read
                        "Run rake task: " tasks)))
    (rake-extract-task-name selected-row)))

(defun rake-select-task ()
  (ido-completing-read "Run rake task: " (rake-get-all-tasks)))

(defun rake-get-all-tasks ()
  "Get list of all available rake tasks"
  (loop
     with raw = (shell-command-to-string "rake --prereqs --silent")
     for line in (split-string raw "\n")
     if (string-match-p "^rake " line)
     collect (replace-regexp-in-string "^rake " "" line)))

;;;###autoload
(defun rake-run-task (all-tasks-p)
  (interactive "P")
  (let* ((task (if all-tasks-p
                   (rake-select-task)
                   (rake-select-documented-task)))
         (command (format "rake %s" task)))
    (shell-command command)))

;;;###autoload
(defun rake-goto-task-definition ()
  (interactive)
  (let* ((task (rake-select-documented-task))
         (command (format "rake --silent --where %s" task))
         (output (shell-command-to-string command)))
    (unless (string-match
             "^rake [^\s]+\s+\\([^\s]+?\\):\\(.+\\):in "
             output)
      (error (format "Failed to find source for task %s" task)))
    (let ((file (match-string 1 output))
          (line (string-to-int (match-string 2 output))))
      (find-file file)
      (goto-line line))))
(provide 'rake)
