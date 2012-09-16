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
(defun rake-run-task (all-tasks)
  (interactive "P")
  (let* ((task (if all-tasks
                   (rake-select-task)
                   (rake-select-documented-task)))
         (command (format "rake %s" task)))
    (shell-command command)))
