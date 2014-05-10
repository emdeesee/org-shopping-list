(provide 'org-shopping-list)

(defun map-regexp (buffer pattern fn)
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


(setq mdc/org-shopping-list-item-pattern
      "^*+\s+\\\(HAVE\\|NEED\\\)\s+\\\(\\w+\\\(\s+\\w+\\\)*\\\)")

(setq mdc/org-shopping-list-file "~/Org/shopping-list.org")

(defun mdc/org-shopping-list-known-items ()
  (let ((shopping-list-buffer
	 (find-file-noselect mdc/org-shopping-list-file)))
    (map-regexp shopping-list-buffer mdc/org-shopping-list-item-pattern
		(lambda (_)
		  (let ((item (match-string 2)))
		    (when item
			(substring-no-properties item)))))))

(defun mdc/org-shopping-list-find-item (item)
  (with-current-buffer (find-file-noselect mdc/org-shopping-list-file)
    (let ((pattern (concat "^*+\s+\\w+\s+" item "\\>\\W*\\(:[^\n]*\\)?$")))
      (goto-char (point-min))
      (re-search-forward pattern nil :noerror))))

(defun mdc/org-shopping-list-add (item)
  (interactive "MItem: ")
  (with-current-buffer (find-file-noselect mdc/org-shopping-list-file)
    (let ((match (mdc/org-shopping-list-find-item item)))
      (if match
	  (goto-char match)
	(progn
	  (goto-char (point-max))
	  (newline)
	  (org-insert-heading)
	  (insert item)))
      (org-todo "NEED")
      (write-file (buffer-file-name)))))

(defun mdc/org-inactive-timestamp nil
  "Generate an org inactive timestamp string for \"now\""
  ;; Org doesn't expose it's timestamps as strings
  (let ((fmt (concat "[" (substring (car org-time-stamp-formats) 1 -1) "]")))
    (format-time-string fmt)))

(defun mdc/org-shopping-list-purchase? (event)
  "Is the event being sent by `org-trigger-hook' a purchase?"
  (and (eq (plist-get event :type) 'todo-state-change)
             (string= (plist-get event :from) "NEED")
             (string= (plist-get event :to) "HAVE")))

(defun mdc/org-shopping-list-on-purchase (event)
  "Update the \"purchase\" property of an item"
  (when (mdc/org-shopping-list-purchase? event)
    (org-set-property "Purchased" (mdc/org-inactive-timestamp))))

(add-hook 'org-trigger-hook 'mdc/org-shopping-list-on-purchase)
