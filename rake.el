;;; rake.el --- rake task runner for GNU Emacs

;; Copyright (C) 2012 Victor Deryagin

;; Author: Victor Deryagin <vderyagin@gmail.com>
;; Created: 16 Sep 2012
;; Version: 0.0.1

;; This file is not part of GNU Emacs.

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
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'compile)
(require 'ansi-color)
(require 'ido)

(defvar rake--tasks-scope nil)

(defun rake-extract-task-name (line)
  (when (string-match "^\\(.+?\\)\s+?# .+" line)
    (match-string 1 line)))

(defun rake-tasks-with-comments ()
  "Return list of rake tasks (with comments) for current location."
  (let* ((command
          (list "rake" "--tasks" "--silent" rake--tasks-scope))
         (output
          (shell-command-to-string (mapconcat 'identity command " "))))
    (loop
       for line in (split-string output "\n")
       if (string-match-p "^rake " line)
       collect (replace-regexp-in-string "^rake " "" line))))

(defun rake-select-task ()
  (let* ((tasks
          (rake-tasks-with-comments))
         (ido-decorations
          '("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]"))
         (selected
          (ido-completing-read "Run rake task: " tasks
                               nil 'require-match nil nil
                               (format "%-25s # %s"
                                       "default"
                                       "Run default task if available."))))
    (rake-extract-task-name selected)))

(defun rake-find-rakefile-directory ()
  (let ((rakefile (rake-find-rakefile)))
    (if rakefile
        (file-name-directory rakefile)
        (error "No Rakefile found (looking for: rakefile, Rakefile, rakefile.rb, Rakefile.rb)"))))

(defun rake-find-rakefile ()
  (loop
     with current-dir = (file-name-as-directory default-directory)
     until (equal current-dir "/")
     thereis (loop
                for file-name in '("rakefile" "Rakefile" "rakefile.rb" "Rakefile.rb")
                for rakefile = (expand-file-name file-name current-dir)
                if (file-regular-p rakefile) return rakefile)
     do
       (setq current-dir
             (file-name-as-directory
              (expand-file-name ".." current-dir)))))

;;;###autoload
(defun rake (&optional global)
  (interactive "P")
  (let* ((rake--tasks-scope (if global "--system" "--no-system"))
         (default-directory
          (or
           (and
            (not global)
            (rake-find-rakefile-directory))
           default-directory))
         (task
          (rake-select-task))
         (command
          (mapconcat 'identity
                     (list "rake" rake--tasks-scope task)
                     " ")))
    (compilation-start command 'rake-mode)))

;;;###autoload
(defun rake-goto-task-definition (&optional global)
  (interactive "P")
  (let* ((rake--tasks-scope (if global "--system" "--no-system"))
         (task (rake-select-task))
         (command (mapconcat 'identity (list "rake" rake--tasks-scope "--silent" "--where" task) " "))
         (output (shell-command-to-string command)))
    (unless (string-match
             "^rake [^\s]+\s+\\([^\s]+?\\):\\(.+\\):in "
             output)
      (error (format "Failed to find source for task %s" task)))
    (let ((file (match-string 1 output))
          (line (string-to-number (match-string 2 output))))
      (unless (file-exists-p file)
        (error (format "File %s does not exist" file)))
      (find-file file)
      (goto-char (point-min))
      (forward-line (1- line))
      (recenter-top-bottom))))


(define-compilation-mode rake-mode "Rake"
  "Mode for running rake tasks."
  (set (make-local-variable 'compilation-scroll-output) t)
  (add-hook 'compilation-filter-hook
            (lambda ()
              (let ((inhibit-read-only t))
                (ansi-color-apply-on-region (point-min) (point-max))))
            nil
            'make-it-local))

(provide 'rake)

;;; rake.el ends here

;; Local Variables:
;; lexical-binding: t
;; coding: us-ascii
;; End:
