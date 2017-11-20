;; Implement a simple shopping list using org-mode tools.  The
;; shopping list is a todo list with the todo keywords HAVE and
;; NEED. A list of needed items can be viewed by using the org-mode
;; agenda commands, C-c a S NEED [RET].
;;
;; Copyright 2014-2017 Michael Cornelius
;; This file is part of org-shopping-list.
;;
;; org-shopping-list is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; org-shopping-list is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with org-shopping-list.  If not, see
;; <http://www.gnu.org/licenses/>.

(provide 'org-shopping-list)

(defun map-regexp (buffer pattern fn)
  "Apply FN to each match for PATTERN in BUFFER"
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let (res)
	(save-match-data
	  (while (re-search-forward pattern nil t)
	    (let ((f (match-data)))
	      (setq res
		    (append res
			    (list
			     (save-match-data
			       (funcall fn f))))))))
	res))))


(setq org-shopping-list-item-pattern
      "^*+\s+\\\(HAVE\\|NEED\\\)\s+\\\(\\w+\\\(\s+\\w+\\\)*\\\)")

(defvar org-shopping-list-file "~/Org/shopping-list.org"
  "Designates the file to use for storing the shopping list.")

(defun org-shopping-list-known-items ()
  (let ((shopping-list-buffer
	 (find-file-noselect org-shopping-list-file)))
    (map-regexp shopping-list-buffer org-shopping-list-item-pattern
		(lambda (_)
		  (let ((item (match-string 2)))
		    (when item
			(substring-no-properties item)))))))

(defun org-shopping-list-find-item (item)
  (with-current-buffer (find-file-noselect org-shopping-list-file)
    (let ((pattern (concat "^*+\s+\\w+\s+" item "\\>\\W*\\(:[^\n]*\\)?$")))
      (goto-char (point-min))
      (re-search-forward pattern nil :noerror))))

(defun org-shopping-list-add (item)
  (interactive "MItem: ")
  (with-current-buffer (find-file-noselect org-shopping-list-file)
    (let ((match (org-shopping-list-find-item item)))
      (if match
	  (goto-char match)
	(progn
	  (goto-char (point-max))
	  (newline)
	  (org-insert-heading)
	  (insert item)))
      (org-todo "NEED")
      (write-file (buffer-file-name)))))

(defun org-inactive-timestamp nil
  "Generate an org inactive timestamp string for \"now\""
  ;; Org doesn't expose it's timestamps as strings
  (let ((fmt (concat "[" (substring (car org-time-stamp-formats) 1 -1) "]")))
    (format-time-string fmt)))

(defun org-shopping-list-purchase? (event)
  "Is the event being sent by `org-trigger-hook' a purchase?"
  (and (eq (plist-get event :type) 'todo-state-change)
             (string= (plist-get event :from) "NEED")
             (string= (plist-get event :to) "HAVE")))

(defun org-shopping-list-on-purchase (event)
  "Update the \"purchase\" property of an item"
  (when (org-shopping-list-purchase? event)
    (org-set-property "Purchased" (org-inactive-timestamp))))

(defun org-shopping-list-show ()
  "Display the list of currently needed items as an org-agenda
todo list."
  (interactive)
  (org-todo-list "NEED")
  (delete-other-windows))

(add-hook 'org-trigger-hook 'org-shopping-list-on-purchase)
